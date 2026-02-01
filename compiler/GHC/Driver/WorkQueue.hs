{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

-- | Global work queue for parallel compilation.
--
-- Replaces the thread-per-module + semaphore model with a fixed worker pool
-- pulling tasks from a priority queue. Each compilation phase (typecheck,
-- codegen) is a separate task. Workers pull from a priority queue, prioritizing
-- typecheck tasks (which unblock dependents) over codegen tasks.
--
-- Enabled by @-fwork-queue@. Without the flag, the traditional pipeline runs.
--
-- See Note [Work Queue Architecture] for details.
module GHC.Driver.WorkQueue
  ( -- * Task types
    TaskEntry(..)
  , TaskPhase(..)
  , phasePriority
    -- * Work queue
  , WorkQueue(..)
  , newEmptyWorkQueue
  , populateWorkQueue
    -- * Running the work queue
  , runWorkQueueWorkers
    -- * Early completion signaling
  , earlyComplete
  ) where

import GHC.Prelude

import GHC.Driver.MakeAction (MakeEnv(..), WorkerLimit(..))

import GHC.Unit.Module.Graph (TaskKey(..))

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import qualified Control.Monad.Catch as MC
import Data.List (partition, sortBy)
import qualified Data.Set as Set

{-
Note [Work Queue Architecture]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The work queue replaces GHC's traditional thread-per-module + semaphore model
with a global task queue and fixed worker pool.

Key insight: Fingerprints depend only on declaration structure, NOT on codegen
info. So early interfaces (from two-phase compilation) are sufficient for
everything except codegen of the SAME module. This means:

  TCTask(M) depends on: TCTask(deps of M)    -- only needs early interfaces
  CGTask(M) depends on: TCTask(M) ONLY       -- NOT deps' CGTasks!

All codegen is completely independent across modules. The critical path is
purely typecheck. Codegen is pure throughput work.

The work queue:
- Has a fixed pool of N workers (= -jN)
- Workers pull from a priority queue
- Typecheck tasks have higher priority than codegen (they unblock dependents)
- No semaphore needed: worker count IS the parallelism limit
- No thread-per-module overhead

Note [Early Completion Signaling]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Each module runs typecheck+codegen as a single task action, but dependents only
need the early interface (produced after typecheck) to start. To avoid waiting
for codegen to finish, we use "early completion":

1. Each module's task depends on its deps' TaskKey_TC keys
2. After typecheck completes (early signal fires), the task calls
   earlyComplete to mark its own TaskKey_TC as satisfied
3. This immediately promotes dependents to the ready queue
4. The task continues with codegen, but dependents are already running

This achieves the same parallelism as split TC/CG tasks without requiring
separate pipeline functions or inter-task communication via MVars.

The wq_in_flight counter tracks tasks that are executing but haven't returned.
This prevents the done condition from triggering while workers are still doing
codegen after their keys have been early-completed.

Task priorities:
  TypecheckPhase = 1000 (highest: unblocks dependents)
  CodegenPhase   = 0    (lowest: doesn't block anyone)
  + fan_out bonus (how many modules depend on this one)
  + critical_path * 2 bonus (distance from leaves)
-}

------------------------------------------------------------------------
-- * Task types
------------------------------------------------------------------------

-- | A phase in the compilation pipeline
data TaskPhase
  = TypecheckPhase      -- ^ Typecheck + desugar (produces early interface)
  | CodegenPhase        -- ^ Code generation (produces .o / .hi)
  | LinkPhase           -- ^ Linking
  | InstantiationPhase  -- ^ Backpack instantiation
  deriving (Eq, Ord, Show)

-- | Base priority for a phase
phasePriority :: TaskPhase -> Int
phasePriority TypecheckPhase     = 1000  -- Highest: unblocks dependents
phasePriority InstantiationPhase = 500
phasePriority CodegenPhase       = 0    -- Lowest: doesn't block anyone
phasePriority LinkPhase          = 0

-- | An entry in the work queue. The @te_action@ is an IO action that
-- performs the actual compilation. This keeps the work queue generic
-- and avoids it needing to know about GHC internals.
data TaskEntry = TaskEntry
  { te_priority  :: {-# UNPACK #-} !Int  -- ^ Higher = run sooner
  , te_phase     :: !TaskPhase
  , te_node_key  :: !TaskKey              -- ^ Unique ID for dep tracking
  , te_deps      :: ![TaskKey]            -- ^ Tasks this depends on
  , te_action    :: !(MakeEnv -> IO ())   -- ^ The compilation action
  }

------------------------------------------------------------------------
-- * Work queue state
------------------------------------------------------------------------

data WorkQueue = WorkQueue
  { wq_ready     :: !(TVar [TaskEntry])            -- ^ Ready to run (sorted by priority desc)
  , wq_waiting   :: !(TVar [TaskEntry])             -- ^ Blocked on deps
  , wq_completed :: !(TVar (Set.Set TaskKey))       -- ^ Completed task keys
  , wq_failed    :: !(TVar (Set.Set TaskKey))       -- ^ Failed task keys
  , wq_in_flight :: !(TVar Int)                     -- ^ Tasks currently executing
  , wq_done      :: !(TVar Bool)                    -- ^ All work complete or aborted
  }

-- | Create an empty work queue. Use 'populateWorkQueue' to add tasks.
-- This allows task actions to close over the WorkQueue for early signaling.
newEmptyWorkQueue :: IO WorkQueue
newEmptyWorkQueue =
  WorkQueue
    <$> newTVarIO []
    <*> newTVarIO []
    <*> newTVarIO Set.empty
    <*> newTVarIO Set.empty
    <*> newTVarIO 0
    <*> newTVarIO True  -- done=True until populated

-- | Populate an empty work queue with tasks.
-- Tasks with no dependencies are placed in the ready queue;
-- tasks with dependencies go to the waiting queue.
populateWorkQueue :: WorkQueue -> [TaskEntry] -> IO ()
populateWorkQueue wq all_tasks = atomically $ do
  let (ready, waiting) = partition (null . te_deps) all_tasks
      sorted_ready = sortBy (\a b -> compare (te_priority b) (te_priority a)) ready
  writeTVar (wq_ready wq) sorted_ready
  writeTVar (wq_waiting wq) waiting
  writeTVar (wq_done wq) (null all_tasks)

------------------------------------------------------------------------
-- * Early completion signaling
------------------------------------------------------------------------

-- | Signal that a task key has been satisfied early (before the task action
-- returns). This is used for two-phase compilation: after typecheck completes,
-- we mark the TC key as done so dependents can start immediately while codegen
-- continues in the current worker.
-- See Note [Early Completion Signaling]
earlyComplete :: WorkQueue -> TaskKey -> IO ()
earlyComplete wq key = do
  atomically $ modifyTVar' (wq_completed wq) (Set.insert key)
  promoteWaiting wq

------------------------------------------------------------------------
-- * Worker loop
------------------------------------------------------------------------

workerLoop :: WorkQueue -> MakeEnv -> IO ()
workerLoop wq env = go
  where
    go = do
      mb_task <- atomically $ do
        done <- readTVar (wq_done wq)
        if done
          then return Nothing
          else do
            ready <- readTVar (wq_ready wq)
            case ready of
              [] -> retry  -- Block until tasks become ready or done signaled
              (t:ts) -> do
                writeTVar (wq_ready wq) ts
                modifyTVar' (wq_in_flight wq) (+1)
                return (Just t)
      case mb_task of
        Nothing -> return ()  -- All done or aborted
        Just entry -> do
          -- Check if any dependency failed (propagate failure)
          failed_set <- readTVarIO (wq_failed wq)
          let deps_failed = any (`Set.member` failed_set) (te_deps entry)
          if deps_failed
            then do
              atomically $ do
                modifyTVar' (wq_failed wq) (Set.insert (te_node_key entry))
                modifyTVar' (wq_in_flight wq) (subtract 1)
              promoteWaiting wq
              go
            else do
              -- Execute the task
              result <- MC.try (te_action entry env)
              case result of
                Left (_ :: MC.SomeException) -> do
                  atomically $ do
                    modifyTVar' (wq_failed wq) (Set.insert (te_node_key entry))
                    modifyTVar' (wq_in_flight wq) (subtract 1)
                Right () -> do
                  atomically $ do
                    modifyTVar' (wq_completed wq) (Set.insert (te_node_key entry))
                    modifyTVar' (wq_in_flight wq) (subtract 1)
              promoteWaiting wq
              go

-- | Move tasks from waiting to ready if all their deps are now satisfied
-- (completed or failed). Also checks the done condition.
promoteWaiting :: WorkQueue -> IO ()
promoteWaiting wq = atomically $ do
  waiting <- readTVar (wq_waiting wq)
  completed <- readTVar (wq_completed wq)
  failed <- readTVar (wq_failed wq)
  let satisfied = Set.union completed failed
      isSatisfied te = all (`Set.member` satisfied) (te_deps te)
      (newly_ready, still_waiting) = partition isSatisfied waiting
  writeTVar (wq_waiting wq) still_waiting
  unless (null newly_ready) $ do
    ready <- readTVar (wq_ready wq)
    let sorted_new = sortBy (\a b -> compare (te_priority b) (te_priority a)) newly_ready
    writeTVar (wq_ready wq) (mergeByPriority ready sorted_new)
  -- Check if everything is done: no waiting, no ready, no in-flight
  when (null still_waiting) $ do
    r <- readTVar (wq_ready wq)
    in_flight <- readTVar (wq_in_flight wq)
    when (null r && in_flight == 0) $
      writeTVar (wq_done wq) True

-- | Merge two descending-priority-sorted lists
mergeByPriority :: [TaskEntry] -> [TaskEntry] -> [TaskEntry]
mergeByPriority [] ys = ys
mergeByPriority xs [] = xs
mergeByPriority (x:xs) (y:ys)
  | te_priority x >= te_priority y = x : mergeByPriority xs (y:ys)
  | otherwise                      = y : mergeByPriority (x:xs) ys

------------------------------------------------------------------------
-- * Entry point
------------------------------------------------------------------------

-- | Spawn a fixed pool of worker threads to process the work queue.
-- Blocks until all tasks are complete (or have failed).
-- Returns True if all tasks succeeded, False if any failed.
runWorkQueueWorkers :: WorkerLimit
                    -> WorkQueue
                    -> MakeEnv
                    -> IO Bool  -- ^ True if all succeeded
runWorkQueueWorkers worker_limit wq env = do
  let n_workers = case worker_limit of
        NumProcessorsLimit n -> max 1 n
        JSemLimit _ -> 4  -- Fallback for jsem

  -- Spawn fixed worker pool
  worker_tids <- replicateM n_workers $ forkIO $ workerLoop wq env

  -- Wait for completion
  atomically $ do
    done <- readTVar (wq_done wq)
    unless done retry

  -- Cleanup workers
  mapM_ killThread worker_tids

  -- Check results
  failed <- readTVarIO (wq_failed wq)
  return (Set.null failed)
