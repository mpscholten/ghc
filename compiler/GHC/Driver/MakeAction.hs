{-# LANGUAGE CPP #-}
module GHC.Driver.MakeAction
  ( MakeAction(..)
  , MakeEnv(..)
  , RunMakeM
  -- * Running the pipelines
  , runAllPipelines
  , runParPipelines
  , runSeqPipelines
  , runPipelines
  , runPipelinesWithPriority
  -- * Worker limit
  , WorkerLimit(..)
  , mkWorkerLimit
  , runWorkerLimit
  -- * Priority scheduling
  -- See Note [Priority-based Scheduling]
  , ActionPhase(..)
  , PrioritizedAction(..)
  , calculatePriority
  , phasePriority
  , sortByPriority
  , runAllPipelinesWithPriority
  -- * Utility
  , withLoggerHsc
  , withParLog
  , withLocalTmpFS
  , withLocalTmpFSMake
  ) where

import GHC.Prelude
import GHC.Driver.DynFlags

import GHC.Driver.Monad
import GHC.Driver.Env
import GHC.Driver.Errors.Types
import GHC.Driver.Messager
import GHC.Driver.MakeSem
import GHC.Unit.Module.Graph (NodeKey)

import GHC.Utils.Logger
import GHC.Utils.TmpFs

import Control.Concurrent ( newQSem, waitQSem, signalQSem, ThreadId, killThread, forkIOWithUnmask )
import qualified GHC.Conc as CC
import Control.Concurrent.MVar
import Control.Monad
import qualified Control.Monad.Catch as MC

import GHC.Conc ( getNumProcessors, getNumCapabilities, setNumCapabilities )
import Control.Monad.Trans.Reader
import GHC.Driver.Pipeline.LogQueue
import Control.Concurrent.STM
import Control.Monad.Trans.Maybe
import Data.List (sortBy)
import Data.Ord (Down(..))
import qualified Data.Set as Set

-- Executing the pipelines

--------------------------------------------------------------------------------
-- * Priority-based Scheduling
-- See Note [Priority-based Scheduling]
--------------------------------------------------------------------------------

{- Note [Priority-based Scheduling]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When building with -j, the scheduler decides which ready modules to compile
next. A smart ordering can significantly reduce build time by:

1. Prioritizing modules with high "fan-out" (many dependents) - completing
   these early unblocks more parallel work.

2. Prioritizing typechecking over code generation - dependent modules only
   need the interface (from typechecking) to start, not the full codegen.
   This works with two-phase interface generation (see Note [Two-phase interface
   generation] in GHC.Driver.Make).

The priority formula is:
  priority = phasePriority(phase) + fanOut

Where:
- phasePriority gives TypecheckPhase a large bonus (1000) over CodegenPhase (0)
- fanOut is the number of modules that transitively depend on this one

Higher priority = run first.

For determinism, we use a secondary sort by module name when priorities are equal.

See #14095 for the original issue and discussion.
-}

-- | The phase of compilation for an action.
-- Used to prioritize typechecking over codegen.
data ActionPhase
  = TypecheckPhase  -- ^ Typechecking phase (produces interface)
  | CodegenPhase    -- ^ Code generation phase (produces object code)
  | OtherPhase      -- ^ Other actions (link nodes, instantiation nodes)
  deriving (Eq, Ord, Show)

-- | An action annotated with priority information for the scheduler.
data PrioritizedAction = PrioritizedAction
  { pa_priority   :: !Int
    -- ^ Higher priority = more urgent. Computed from phase + fan-out.
  , pa_phase      :: !ActionPhase
    -- ^ The compilation phase (for debugging/logging)
  , pa_sort_key   :: !String
    -- ^ Secondary sort key for determinism (typically module name)
  , pa_node_key   :: !NodeKey
    -- ^ The NodeKey identifying this action (for dependency tracking)
  , pa_deps       :: ![NodeKey]
    -- ^ Direct dependencies (NodeKeys this action waits for)
  , pa_action     :: !MakeAction
    -- ^ The underlying action to execute
  }

instance Eq PrioritizedAction where
  a == b = pa_priority a == pa_priority b && pa_sort_key a == pa_sort_key b

instance Ord PrioritizedAction where
  -- Higher priority first, then lexicographic by sort key for determinism
  compare a b = compare (Down (pa_priority a), pa_sort_key a)
                        (Down (pa_priority b), pa_sort_key b)

-- | Base priority for each phase.
-- TypecheckPhase gets a large bonus because completing typecheck
-- allows dependent modules to start their own typecheck.
phasePriority :: ActionPhase -> Int
phasePriority TypecheckPhase = 1000  -- High priority
phasePriority OtherPhase     = 500   -- Medium priority (link nodes, etc.)
phasePriority CodegenPhase   = 0     -- Low priority

-- | Calculate the priority for an action.
-- Priority = phase priority + fan-out score.
calculatePriority :: ActionPhase -> Int -> Int
calculatePriority phase fanOut = phasePriority phase + fanOut

-- | Sort a list of prioritized actions by priority (highest first).
-- For equal priorities, uses sort_key for deterministic ordering.
sortByPriority :: [PrioritizedAction] -> [PrioritizedAction]
sortByPriority = sortBy compare

mkWorkerLimit :: DynFlags -> IO WorkerLimit
mkWorkerLimit dflags =
  case parMakeCount dflags of
    Nothing -> pure $ num_procs 1
    Just (ParMakeSemaphore h) -> pure (JSemLimit (SemaphoreName h))
    Just ParMakeNumProcessors -> num_procs <$> getNumProcessors
    Just (ParMakeThisMany n) -> pure $ num_procs n
  where
    num_procs x = NumProcessorsLimit (max 1 x)

isWorkerLimitSequential :: WorkerLimit -> Bool
isWorkerLimitSequential (NumProcessorsLimit x) = x <= 1
isWorkerLimitSequential (JSemLimit {})         = False

-- | This describes what we use to limit the number of jobs, either we limit it
-- ourselves to a specific number or we have an external parallelism semaphore
-- limit it for us.
data WorkerLimit
  = NumProcessorsLimit Int
  | JSemLimit
    SemaphoreName
      -- ^ Semaphore name to use
  deriving Eq

-- | Environment used when compiling a module
data MakeEnv = MakeEnv { hsc_env :: !HscEnv -- The basic HscEnv which will be augmented for each module
                       , compile_sem :: !AbstractSem
                       -- Modify the environment for module k, with the supplied logger modification function.
                       -- For -j1, this wrapper doesn't do anything
                       -- For -jn, the wrapper initialised a log queue and then modifies the logger to pipe its output
                       --          into the log queue.
                       , withLogger :: forall a . Int -> ((Logger -> Logger) -> IO a) -> IO a
                       , env_messager :: !(Maybe Messager)
                       , diag_wrapper :: GhcMessage -> AnyGhcDiagnostic
                       }


label_self :: String -> IO ()
label_self thread_name = do
    self_tid <- CC.myThreadId
    CC.labelThread self_tid thread_name


runPipelines :: WorkerLimit -> HscEnv -> (GhcMessage -> AnyGhcDiagnostic) -> Maybe Messager -> [MakeAction] -> IO ()
-- Don't even initialise plugins if there are no pipelines
runPipelines n_job hsc_env diag_wrapper mHscMessager all_pipelines = do
  liftIO $ label_self "main --make thread"
  case n_job of
    NumProcessorsLimit n | n <= 1 -> runSeqPipelines hsc_env diag_wrapper mHscMessager all_pipelines
    _n -> runParPipelines n_job hsc_env diag_wrapper mHscMessager all_pipelines

-- | Run prioritized pipelines, sorting by priority before execution.
-- For parallel builds, higher priority actions are started first.
-- See Note [Priority-based Scheduling]
runPipelinesWithPriority :: WorkerLimit -> HscEnv -> (GhcMessage -> AnyGhcDiagnostic) -> Maybe Messager -> [PrioritizedAction] -> IO ()
runPipelinesWithPriority n_job hsc_env diag_wrapper mHscMessager all_pipelines = do
  liftIO $ label_self "main --make thread"
  case n_job of
    NumProcessorsLimit n | n <= 1 ->
      -- For sequential builds, priority doesn't matter much (topological order is respected)
      runSeqPipelines hsc_env diag_wrapper mHscMessager (map pa_action all_pipelines)
    _n -> runParPipelinesWithPriority n_job hsc_env diag_wrapper mHscMessager all_pipelines

runSeqPipelines :: HscEnv -> (GhcMessage -> AnyGhcDiagnostic) -> Maybe Messager -> [MakeAction] -> IO ()
runSeqPipelines plugin_hsc_env diag_wrapper mHscMessager all_pipelines =
  let env = MakeEnv { hsc_env = plugin_hsc_env
                    , withLogger = \_ k -> k id
                    , compile_sem = AbstractSem (return ()) (return ())
                    , env_messager = mHscMessager
                    , diag_wrapper = diag_wrapper
                    }
  in runAllPipelines (NumProcessorsLimit 1) env all_pipelines

runNjobsAbstractSem :: Int -> (AbstractSem -> IO a) -> IO a
runNjobsAbstractSem n_jobs action = do
  compile_sem <- newQSem n_jobs
  n_capabilities <- getNumCapabilities
  n_cpus <- getNumProcessors
  let
    asem = AbstractSem (waitQSem compile_sem) (signalQSem compile_sem)
    set_num_caps n = unless (n_capabilities /= 1) $ setNumCapabilities n
    updNumCapabilities =  do
      -- Setting number of capabilities more than
      -- CPU count usually leads to high userspace
      -- lock contention. #9221
      set_num_caps $ min n_jobs n_cpus
    resetNumCapabilities = set_num_caps n_capabilities
  MC.bracket_ updNumCapabilities resetNumCapabilities $ action asem

runWorkerLimit :: WorkerLimit -> (AbstractSem -> IO a) -> IO a
#if defined(wasm32_HOST_ARCH)
runWorkerLimit _ action = do
  lock <- newMVar ()
  action $ AbstractSem (takeMVar lock) (putMVar lock ())
#else
runWorkerLimit worker_limit action = case worker_limit of
    NumProcessorsLimit n_jobs ->
      runNjobsAbstractSem n_jobs action
    JSemLimit sem ->
      runJSemAbstractSem sem action
#endif

-- | Build and run a pipeline
runParPipelines :: WorkerLimit -- ^ How to limit work parallelism
             -> HscEnv         -- ^ The basic HscEnv which is augmented with specific info for each module
             -> (GhcMessage -> AnyGhcDiagnostic)
             -> Maybe Messager   -- ^ Optional custom messager to use to report progress
             -> [MakeAction]  -- ^ The build plan for all the module nodes
             -> IO ()
runParPipelines worker_limit plugin_hsc_env diag_wrapper mHscMessager all_pipelines = do


  -- A variable which we write to when an error has happened and we have to tell the
  -- logging thread to gracefully shut down.
  stopped_var <- newTVarIO False
  -- The queue of LogQueues which actions are able to write to. When an action starts it
  -- will add it's LogQueue into this queue.
  log_queue_queue_var <- newTVarIO newLogQueueQueue
  -- Thread which coordinates the printing of logs
  wait_log_thread <- logThread (hsc_logger plugin_hsc_env) stopped_var log_queue_queue_var


  -- Make the logger thread-safe, in case there is some output which isn't sent via the LogQueue.
  thread_safe_logger <- liftIO $ makeThreadSafe (hsc_logger plugin_hsc_env)
  let thread_safe_hsc_env = plugin_hsc_env { hsc_logger = thread_safe_logger }

  runWorkerLimit worker_limit $ \abstract_sem -> do
    let env = MakeEnv { hsc_env = thread_safe_hsc_env
                      , withLogger = withParLog log_queue_queue_var
                      , compile_sem = abstract_sem
                      , env_messager = mHscMessager
                      , diag_wrapper = diag_wrapper
                      }
    -- Reset the number of capabilities once the upsweep ends.
    runAllPipelines worker_limit env all_pipelines
    atomically $ writeTVar stopped_var True
    wait_log_thread

-- | Build and run a pipeline with priority-based scheduling.
-- Actions are sorted by priority before execution, so higher priority
-- modules are started first.
-- See Note [Priority-based Scheduling]
runParPipelinesWithPriority :: WorkerLimit -- ^ How to limit work parallelism
             -> HscEnv         -- ^ The basic HscEnv which is augmented with specific info for each module
             -> (GhcMessage -> AnyGhcDiagnostic)
             -> Maybe Messager   -- ^ Optional custom messager to use to report progress
             -> [PrioritizedAction]  -- ^ The build plan with priority info
             -> IO ()
runParPipelinesWithPriority worker_limit plugin_hsc_env diag_wrapper mHscMessager all_pipelines = do

  -- A variable which we write to when an error has happened and we have to tell the
  -- logging thread to gracefully shut down.
  stopped_var <- newTVarIO False
  -- The queue of LogQueues which actions are able to write to. When an action starts it
  -- will add it's LogQueue into this queue.
  log_queue_queue_var <- newTVarIO newLogQueueQueue
  -- Thread which coordinates the printing of logs
  wait_log_thread <- logThread (hsc_logger plugin_hsc_env) stopped_var log_queue_queue_var

  -- Make the logger thread-safe, in case there is some output which isn't sent via the LogQueue.
  thread_safe_logger <- liftIO $ makeThreadSafe (hsc_logger plugin_hsc_env)
  let thread_safe_hsc_env = plugin_hsc_env { hsc_logger = thread_safe_logger }

  runWorkerLimit worker_limit $ \abstract_sem -> do
    let env = MakeEnv { hsc_env = thread_safe_hsc_env
                      , withLogger = withParLog log_queue_queue_var
                      , compile_sem = abstract_sem
                      , env_messager = mHscMessager
                      , diag_wrapper = diag_wrapper
                      }
    -- Use priority-based scheduling: sort actions by priority before running
    -- This ensures higher priority modules (high fan-out, typecheck phase) run first
    runAllPipelinesWithPriority worker_limit env all_pipelines
    atomically $ writeTVar stopped_var True
    wait_log_thread

withLoggerHsc :: Int -> MakeEnv -> (HscEnv -> IO a) -> IO a
withLoggerHsc k MakeEnv{withLogger, hsc_env} cont = do
  withLogger k $ \modifyLogger -> do
    let lcl_logger = modifyLogger (hsc_logger hsc_env)
        hsc_env' = hsc_env { hsc_logger = lcl_logger }
    -- Run continuation with modified logger
    cont hsc_env'

withParLog :: TVar LogQueueQueue -> Int -> ((Logger -> Logger) -> IO b) -> IO b
withParLog lqq_var k cont = do
  let init_log = do
        -- Make a new log queue
        lq <- newLogQueue k
        -- Add it into the LogQueueQueue
        atomically $ initLogQueue lqq_var lq
        return lq
      finish_log lq = liftIO (finishLogQueue lq)
  MC.bracket init_log finish_log $ \lq -> cont (pushLogHook (const (parLogAction lq)))

withLocalTmpFS :: TmpFs -> (TmpFs -> IO a) -> IO a
withLocalTmpFS tmpfs act = do
  let initialiser = do
        liftIO $ forkTmpFsFrom tmpfs
      finaliser tmpfs_local = do
        liftIO $ mergeTmpFsInto tmpfs_local tmpfs
       -- Add remaining files which weren't cleaned up into local tmp fs for
       -- clean-up later.
       -- Clear the logQueue if this node had it's own log queue
  MC.bracket initialiser finaliser act

withLocalTmpFSMake :: MakeEnv -> (MakeEnv -> IO a) -> IO a
withLocalTmpFSMake env k =
  withLocalTmpFS (hsc_tmpfs (hsc_env env)) $ \lcl_tmpfs
    -> k (env { hsc_env = (hsc_env env) { hsc_tmpfs = lcl_tmpfs }})


-- | Run the given actions and then wait for them all to finish.
-- Actions are processed in the order provided, with the expectation that
-- callers have sorted them by priority if desired.
-- See Note [Priority-based Scheduling]
runAllPipelines :: WorkerLimit -> MakeEnv -> [MakeAction] -> IO ()
runAllPipelines worker_limit env acts = do
  let single_worker = isWorkerLimitSequential worker_limit
      spawn_actions :: IO [ThreadId]
      spawn_actions = if single_worker
        then (:[]) <$> (forkIOWithUnmask $ \unmask -> void $ runLoop (\io -> io unmask) env acts)
        else runLoop forkIOWithUnmask env acts

      kill_actions :: [ThreadId] -> IO ()
      kill_actions tids = mapM_ killThread tids

  MC.bracket spawn_actions kill_actions $ \_ -> do
    mapM_ waitMakeAction acts

-- | Run prioritized actions, sorting by priority first.
-- Higher priority actions are started first, which typically means they
-- get to the semaphore earlier when their dependencies are satisfied.
-- See Note [Priority-based Scheduling]
--
-- Note: We use static scheduling (sort once, spawn all) rather than dynamic
-- scheduling because GHC's MVar-based dependency waiting already handles
-- ordering correctly. Dynamic scheduling adds overhead without benefit since
-- actions block on their dependency MVars regardless of spawn order.
runAllPipelinesWithPriority :: WorkerLimit -> MakeEnv -> [PrioritizedAction] -> IO ()
runAllPipelinesWithPriority worker_limit env pacts = do
  let sorted_acts = map pa_action (sortByPriority pacts)
  runAllPipelines worker_limit env sorted_acts

{- Note [Dynamic Priority Scheduling]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The dynamic scheduler improves on static priority sorting by re-evaluating
priorities when dependencies complete.

Static scheduling problem:
  - All actions are spawned at the start in priority order
  - Actions block internally on their dependencies
  - When deps complete, the action continues, but lower-priority actions
    may have already acquired the semaphore

Dynamic scheduling solution:
  - Track which actions are "ready" (all deps satisfied) vs "waiting"
  - Only spawn ready actions
  - When an action completes, check waiting actions and spawn newly-ready ones
  - Newly-ready actions compete fairly based on current priority

This helps in cases like:
  - Module A (high priority) depends on B
  - Module C (low priority) has no deps
  - With static scheduling: C starts first, may delay A
  - With dynamic scheduling: C starts, B starts, when B completes A starts immediately

The benefit increases with:
  - Higher parallelism (-j8 and above)
  - Deeper dependency chains
  - More "bursty" completion patterns (one module unblocking many)
-}

-- | State for the dynamic scheduler
data DynamicSchedulerState = DynamicSchedulerState
  { dss_completed :: !(TVar (Set.Set NodeKey))
    -- ^ Actions that have completed
  , dss_waiting   :: !(TVar [PrioritizedAction])
    -- ^ Actions waiting for dependencies (kept sorted by priority)
  , dss_threads   :: !(TVar [ThreadId])
    -- ^ Spawned thread IDs (for cleanup)
  , dss_all_done  :: !(TVar Bool)
    -- ^ Signal that all actions are complete
  }

-- | Run pipelines with dynamic priority scheduling.
-- Actions are spawned as their dependencies become satisfied.
runDynamicPipelines :: MakeEnv -> [PrioritizedAction] -> IO ()
runDynamicPipelines env pacts = do
  -- Initialize scheduler state
  completed_var <- newTVarIO Set.empty
  waiting_var <- newTVarIO []
  threads_var <- newTVarIO []
  all_done_var <- newTVarIO False

  let state = DynamicSchedulerState
        { dss_completed = completed_var
        , dss_waiting = waiting_var
        , dss_threads = threads_var
        , dss_all_done = all_done_var
        }

  -- Build set of all action node keys (deps outside this set are "external" and already satisfied)
  let actionKeys = Set.fromList (map pa_node_key pacts)

  -- Partition into ready (no in-build deps) and waiting (has in-build deps)
  let (ready, waiting) = partitionReady Set.empty actionKeys pacts

  -- Initialize waiting queue (sorted by priority)
  atomically $ writeTVar waiting_var (sortByPriority waiting)

  -- Spawn initial ready actions
  mapM_ (spawnAction state env actionKeys) (sortByPriority ready)

  -- Wait for all actions to complete
  atomically $ do
    done <- readTVar all_done_var
    unless done retry

  -- All done - threads are already cleaned up by their completion handlers
  return ()

  where
    total_actions = length pacts

    -- | Check if an action is ready (all in-build deps in completed set)
    -- External deps (not in actionKeys) are considered already satisfied
    isReady :: Set.Set NodeKey -> Set.Set NodeKey -> PrioritizedAction -> Bool
    isReady completed actionKeys pa =
      let inBuildDeps = filter (`Set.member` actionKeys) (pa_deps pa)
      in all (`Set.member` completed) inBuildDeps

    -- | Partition actions into ready and waiting
    partitionReady :: Set.Set NodeKey -> Set.Set NodeKey -> [PrioritizedAction] -> ([PrioritizedAction], [PrioritizedAction])
    partitionReady completed actionKeys = foldr go ([], [])
      where
        go pa (ready, waiting)
          | isReady completed actionKeys pa = (pa : ready, waiting)
          | otherwise = (ready, pa : waiting)

    -- | Spawn an action and set up completion callback
    spawnAction :: DynamicSchedulerState -> MakeEnv -> Set.Set NodeKey -> PrioritizedAction -> IO ()
    spawnAction state env actionKeys pa =
      case pa_action pa of
        MakeAction act res_var -> do
          tid <- withLocalTmpFSMake env $ \lcl_env ->
            forkIOWithUnmask $ \unmask -> do
              mres <- (unmask $ runMaybeT (runReaderT act lcl_env))
                        `MC.onException` (putMVar res_var Nothing)
              putMVar res_var mres
              -- Action completed - update scheduler state
              onActionComplete state env actionKeys (pa_node_key pa)

          -- Track thread for potential cleanup
          atomically $ modifyTVar' (dss_threads state) (tid :)

    -- | Called when an action completes
    onActionComplete :: DynamicSchedulerState -> MakeEnv -> Set.Set NodeKey -> NodeKey -> IO ()
    onActionComplete state env actionKeys completed_key = do
      -- Add to completed set and get newly ready actions
      newly_ready <- atomically $ do
        -- Add to completed set
        modifyTVar' (dss_completed state) (Set.insert completed_key)
        completed <- readTVar (dss_completed state)

        -- Check waiting actions for newly ready ones
        waiting <- readTVar (dss_waiting state)
        let (ready, still_waiting) = partitionReady completed actionKeys waiting
        writeTVar (dss_waiting state) still_waiting

        -- Check if all done
        let n_completed = Set.size completed
        let n_waiting = length still_waiting
        when (n_completed >= total_actions || (n_completed + n_waiting == 0 && null ready)) $
          writeTVar (dss_all_done state) True

        return (sortByPriority ready)

      -- Spawn newly ready actions (outside STM)
      mapM_ (spawnAction state env actionKeys) newly_ready

-- | Execute each action in order, limiting the amount of parallelism by the given
-- semaphore.
runLoop :: (((forall a. IO a -> IO a) -> IO ()) -> IO a) -> MakeEnv -> [MakeAction] -> IO [a]
runLoop _ _env [] = return []
runLoop fork_thread env (MakeAction act res_var :acts) = do

  -- withLocalTmpFs has to occur outside of fork to remain deterministic
  new_thread <- withLocalTmpFSMake env $ \lcl_env ->
    fork_thread $ \unmask -> (do
            mres <- (unmask $ run_pipeline lcl_env act)
                      `MC.onException` (putMVar res_var Nothing) -- Defensive: If there's an unhandled exception then still signal the failure.
            putMVar res_var mres)
  threads <- runLoop fork_thread env acts
  return (new_thread : threads)
  where
      run_pipeline :: MakeEnv -> RunMakeM a -> IO (Maybe a)
      run_pipeline env p = runMaybeT (runReaderT p env)

type RunMakeM a = ReaderT MakeEnv (MaybeT IO) a

data MakeAction = forall a . MakeAction !(RunMakeM a) !(MVar (Maybe a))

waitMakeAction :: MakeAction -> IO ()
waitMakeAction (MakeAction _ mvar) = () <$ readMVar mvar