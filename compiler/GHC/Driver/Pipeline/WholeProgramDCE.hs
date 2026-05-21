{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- | Pipeline integration for whole-program dead code elimination
--
-- This module provides the glue between GHC's compilation pipeline
-- and the whole-program DCE analysis. It handles:
--
--   1. Initialization of DCE state at the start of compilation
--   2. Notification when modules finish Core optimization
--   3. Coordination of deferred codegen for bindings with unknown liveness
--   4. Finalization of DCE at link time
--
-- See Note [Whole-Program DCE Pipeline Integration] for details.
module GHC.Driver.Pipeline.WholeProgramDCE
    ( -- * DCE Context
      DCEContext(..)
    , initDCEContext
    , noDCEContext

      -- * Module processing
    , registerModuleForDCE
    , getRootsFromModule

      -- * Codegen coordination
    , shouldCodegenBinding
    , getDeferredBindings
    , codegenDeferredBindings

      -- * Finalization
    , finalizeDCE
    , reportDCEStats

      -- * Global DCE context for codegen hook
    , setGlobalDCEContext
    , registerCoreBindingsForDCE
    , filterCoreBindingsForDCE
    , storeCgGutsForDCE

      -- * Link-time deferred codegen
    , getStoredModuleCores
    , getFilteredCgGutsForModule
    , getModulesWithDeadCode

      -- * Liveness checking for StgToCmm
    , isNameLive
    ) where

import GHC.Prelude

import GHC.Core
import GHC.Core.Opt.WholeProgramDCE
    ( initLiveness, moduleFinishedWithRoots, partitionByLiveness
    , getLiveBindings, filterLiveBindings, finalizeLiveness, checkLiveness
    , LivenessState, LivenessResult(..)
    )
import GHC.Core.Opt.WholeProgramDCE.Types
    ( DeferredBindings(..), emptyLivenessState, StoredModuleCore(..)
    , LivenessState(..)  -- Need ls_finalized field
    )
import GHC.Unit.Module.ModGuts (CgGuts(..))
import GHC.Unit.Module.Location (ModLocation(..))

import GHC.Driver.DynFlags (DynFlags, mainModuleNameIs, mainFunIs)
import GHC.Driver.Session (gopt)
import GHC.Driver.Flags (GeneralFlag(..))

import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Unit.Module

import GHC.Utils.Logger
import GHC.Utils.Error (debugTraceMsg)
import GHC.Utils.Outputable

import Data.IORef
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import Control.Monad (when)
import System.IO.Unsafe (unsafePerformIO)

{-
Note [Whole-Program DCE Pipeline Integration]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The DCE integrates into GHC's compilation pipeline as follows:

1. INITIALIZATION (at start of upsweep):
   - Create DCEContext with empty liveness state
   - Find initial roots from DynFlags (main function, etc.)

2. MODULE COMPILATION (for each module):
   - After Core optimization completes, call registerModuleForDCE
   - This adds bindings to the dependency graph
   - Propagates liveness from known roots
   - Returns which bindings are definitely live (codegen immediately)
     and which are unknown (defer codegen)

3. LINK TIME:
   - Call finalizeDCE to complete the analysis
   - All unknown bindings now become either live or dead
   - Codegen the deferred bindings that turned out live
   - Skip bindings that are dead

The key insight is that codegen can proceed speculatively for definitely-live
bindings, while unknown bindings are deferred. This maintains parallelism
while still eliminating dead code.
-}

-- | Context for whole-program DCE
--
-- This is passed through the compilation pipeline and tracks all
-- the state needed for DCE.
data DCEContext = DCEContext
    { dce_enabled       :: !Bool                              -- ^ Is DCE enabled?
    , dce_liveness      :: !LivenessState                     -- ^ Liveness tracking state
    , dce_deferred      :: !(IORef (M.Map Module DeferredBindings))  -- ^ Deferred bindings by module
    , dce_stats         :: !(IORef DCEStats)                  -- ^ Statistics for reporting
    , dce_module_cores  :: !(IORef (M.Map Module StoredModuleCore))  -- ^ Stored Core for deferred codegen
    }

-- | Statistics about DCE effectiveness
data DCEStats = DCEStats
    { dces_total_bindings   :: !Int   -- ^ Total bindings seen
    , dces_live_bindings    :: !Int   -- ^ Bindings that were live
    , dces_dead_bindings    :: !Int   -- ^ Bindings that were eliminated
    , dces_deferred_bindings :: !Int  -- ^ Bindings that were deferred
    }

emptyDCEStats :: DCEStats
emptyDCEStats = DCEStats 0 0 0 0

-- | Initialize DCE context for a compilation session
initDCEContext :: DynFlags -> IO DCEContext
initDCEContext dflags = do
    liveness <- initLiveness dflags
    deferredRef <- newIORef M.empty
    statsRef <- newIORef emptyDCEStats
    moduleCoresRef <- newIORef M.empty
    return DCEContext
        { dce_enabled      = gopt Opt_WholeProgramDCE dflags
        , dce_liveness     = liveness
        , dce_deferred     = deferredRef
        , dce_stats        = statsRef
        , dce_module_cores = moduleCoresRef
        }

-- | Create a DCE context for when DCE is disabled
noDCEContext :: IO DCEContext
noDCEContext = do
    liveness <- emptyLivenessState
    deferredRef <- newIORef M.empty
    statsRef <- newIORef emptyDCEStats
    moduleCoresRef <- newIORef M.empty
    return DCEContext
        { dce_enabled      = False
        , dce_liveness     = liveness
        , dce_deferred     = deferredRef
        , dce_stats        = statsRef
        , dce_module_cores = moduleCoresRef
        }

-- | Register a module's bindings with DCE after Core optimization
--
-- Returns:
--   - The bindings that are definitely live (codegen immediately)
--   - The bindings that have unknown liveness (deferred)
--
-- When DCE is disabled, all bindings are returned as "live".
registerModuleForDCE
    :: DCEContext
    -> DynFlags
    -> Module
    -> CoreProgram
    -> IO (CoreProgram, CoreProgram)  -- ^ (live bindings, deferred bindings)
registerModuleForDCE ctx dflags mod binds
    | not (dce_enabled ctx) = return (binds, [])  -- DCE disabled, all live
    | otherwise = do
        -- Register with liveness tracker (also identifies roots like main)
        moduleFinishedWithRoots (dce_liveness ctx) dflags mod binds

        -- Partition bindings by liveness
        (live, unknown, _dead) <- partitionByLiveness (dce_liveness ctx) binds

        -- Store deferred bindings
        when (not (null unknown)) $ do
            let deferred = DeferredBindings
                    { db_module   = mod
                    , db_bindings = unknown
                    , db_names    = mkNameSet (concatMap bindNames unknown)
                    }
            modifyIORef' (dce_deferred ctx) (M.insert mod deferred)

        -- Update stats
        modifyIORef' (dce_stats ctx) $ \s -> s
            { dces_total_bindings = dces_total_bindings s + length binds
            , dces_live_bindings = dces_live_bindings s + length live
            , dces_deferred_bindings = dces_deferred_bindings s + length unknown
            }

        return (live, unknown)
  where
    bindNames (NonRec b _) = [idName b]
    bindNames (Rec pairs) = map (idName . fst) pairs

-- | Find root bindings in a module
--
-- This identifies bindings that should be treated as roots:
--   - The main function
--   - Foreign exports
--   - Template Haskell entry points
getRootsFromModule :: DynFlags -> Module -> CoreProgram -> NameSet
getRootsFromModule dflags mod binds = mconcat
    [ mainRoot dflags mod binds
    -- Foreign exports would be added here
    -- TH entry points would be added here
    ]

-- | Find the main function root
mainRoot :: DynFlags -> Module -> CoreProgram -> NameSet
mainRoot dflags mod binds
    | moduleName mod /= mainModuleNameIs dflags = emptyNameSet
    | otherwise =
        let mainName = fromMaybe "main" (mainFunIs dflags)
        in mkNameSet [ idName b
                     | bind <- binds
                     , b <- bindersOf bind
                     , getOccString (idName b) == mainName
                     ]

-- | Check if a specific binding should be compiled
shouldCodegenBinding :: DCEContext -> Name -> IO Bool
shouldCodegenBinding ctx name
    | not (dce_enabled ctx) = return True  -- DCE disabled
    | otherwise = do
        result <- checkLiveness (dce_liveness ctx) name
        return $ case result of
            DefinitelyLive -> True
            DefinitelyDead -> False
            Unknown        -> False  -- Will be handled in deferred codegen

-- | Get all deferred bindings for a module
getDeferredBindings :: DCEContext -> Module -> IO (Maybe DeferredBindings)
getDeferredBindings ctx mod = do
    deferred <- readIORef (dce_deferred ctx)
    return (M.lookup mod deferred)

-- | Codegen deferred bindings that turned out to be live
--
-- Called at link time after finalizeDCE. Returns the bindings
-- that should be compiled (were deferred but are now known live).
codegenDeferredBindings :: DCEContext -> Module -> IO CoreProgram
codegenDeferredBindings ctx mod = do
    mDeferred <- getDeferredBindings ctx mod
    case mDeferred of
        Nothing -> return []
        Just deferred -> do
            live <- getLiveBindings (dce_liveness ctx)
            let liveBindings = filterLiveBindings live (db_bindings deferred)
            return liveBindings

-- | Finalize DCE analysis
--
-- Called at link time when all modules have been processed.
-- After this, all Unknown bindings become either Live or Dead.
finalizeDCE :: DCEContext -> IO NameSet
finalizeDCE ctx
    | not (dce_enabled ctx) = return emptyNameSet
    | otherwise = do
        finalLive <- finalizeLiveness (dce_liveness ctx)

        -- Update stats with final live/dead counts
        modifyIORef' (dce_stats ctx) $ \s ->
            let liveCount = sizeNameSet finalLive
                -- Dead = total - live (everything not in the live set is dead)
                deadCount = max 0 (dces_total_bindings s - liveCount)
            in s { dces_live_bindings = liveCount
                 , dces_dead_bindings = deadCount
                 }

        return finalLive

-- | Report DCE statistics
reportDCEStats :: Logger -> DCEContext -> IO ()
reportDCEStats logger ctx
    | not (dce_enabled ctx) = return ()
    | otherwise = do
        stats <- readIORef (dce_stats ctx)
        let total = dces_total_bindings stats
            live = dces_live_bindings stats
            dead = dces_dead_bindings stats
            pct = if total > 0
                  then (dead * 100) `div` total
                  else 0

        debugTraceMsg logger 1 $
            hang (text "Whole-program DCE:") 2 $ vcat
                [ text "Total bindings:" <+> int total
                , text "Live bindings:" <+> int live
                , text "Dead bindings:" <+> int dead <+> parens (int pct <> text "%")
                ]

-- | Size of a NameSet (for statistics)
sizeNameSet :: NameSet -> Int
sizeNameSet = length . nameSetElemsStable

-- -----------------------------------------------------------------------------
-- Global DCE context for codegen hook
--
-- This is used to make the DCE context available during codegen without
-- threading it through all the compilation functions.

-- | Global IORef holding the current DCE context (if any)
{-# NOINLINE globalDCEContext #-}
globalDCEContext :: IORef (Maybe DCEContext)
globalDCEContext = unsafePerformIO (newIORef Nothing)

-- | Set the global DCE context at the start of compilation
setGlobalDCEContext :: Maybe DCEContext -> IO ()
setGlobalDCEContext ctx = writeIORef globalDCEContext ctx

-- | Register Core bindings with DCE during codegen
--
-- This is called from hscGenHardCode when DCE is enabled.
-- It registers the Core bindings for a module so we can track statistics.
registerCoreBindingsForDCE :: DynFlags -> Module -> CoreProgram -> IO ()
registerCoreBindingsForDCE dflags mod binds = do
    mctx <- readIORef globalDCEContext
    case mctx of
        Nothing -> return ()  -- DCE not enabled
        Just ctx | not (dce_enabled ctx) -> return ()
        Just ctx -> do
            -- Register the module with DCE
            moduleFinishedWithRoots (dce_liveness ctx) dflags mod binds

            -- Update statistics with total bindings count
            let bindCount = length binds
            modifyIORef' (dce_stats ctx) $ \s -> s
                { dces_total_bindings = dces_total_bindings s + bindCount
                }

-- | Filter Core bindings by DCE liveness and return only live bindings
--
-- This is the main entry point for DCE during codegen. It:
--   1. Registers the module's bindings with DCE (propagating liveness)
--   2. Stores Core bindings for non-Main modules (for deferred codegen at link time)
--   3. Returns only the bindings that are definitely live or have unknown liveness
--   4. Eliminates bindings that are definitely dead
--
-- For the Main module, we can be aggressive: after propagating liveness from
-- main, any binding in Main that isn't live is dead (nothing else imports Main).
--
-- For other modules, bindings have "unknown" liveness until all modules
-- are processed. We store their Core so at link time we can regenerate
-- object files with only the live bindings.
--
-- Returns (filtered_bindings, eliminated_count)
filterCoreBindingsForDCE :: DynFlags -> Module -> CoreProgram -> IO (CoreProgram, Int)
filterCoreBindingsForDCE dflags mod binds = do
    mctx <- readIORef globalDCEContext
    case mctx of
        Nothing -> return (binds, 0)  -- DCE not enabled, keep all
        Just ctx | not (dce_enabled ctx) -> return (binds, 0)
        Just ctx -> do
            -- Register the module with DCE (this propagates liveness from roots)
            moduleFinishedWithRoots (dce_liveness ctx) dflags mod binds

            -- Check if this is the Main module - if so, we can be aggressive
            -- because nothing imports Main, so non-live bindings are truly dead
            let isMainModule = moduleName mod == mainModuleNameIs dflags

            -- Get current liveness set
            liveSet <- getLiveBindings (dce_liveness ctx)

            -- Partition bindings: live vs not-live
            let (liveBinds, notLiveBinds) = partitionBinds liveSet binds

            -- For Main module: notLive = dead (nothing else imports Main)
            -- For other modules: notLive = unknown (might be imported by Main)
            let (keptBinds, eliminatedBinds) =
                    if isMainModule
                    then (liveBinds, notLiveBinds)  -- Eliminate non-live in Main
                    else (liveBinds ++ notLiveBinds, [])  -- Keep all in other modules

            -- Update total bindings count (final live/dead calculated at finalization)
            let totalCount = length binds
                eliminatedCount = length eliminatedBinds

            modifyIORef' (dce_stats ctx) $ \s -> s
                { dces_total_bindings = dces_total_bindings s + totalCount
                }

            return (keptBinds, eliminatedCount)
  where
    -- Partition bindings into (live, not-live) based on liveness set
    partitionBinds :: NameSet -> CoreProgram -> ([CoreBind], [CoreBind])
    partitionBinds liveSet = foldr categorize ([], [])
      where
        categorize bind (lives, notLives)
            | any (\b -> idName b `elemNameSet` liveSet) (bindersOf bind)
            = (bind : lives, notLives)
            | otherwise
            = (lives, bind : notLives)

-- | Store CgGuts for a non-Main module for deferred codegen at link time
--
-- This should be called from hscGenHardCode before filtering Core bindings.
-- We store the full CgGuts and ModLocation so we can regenerate object files
-- with dead code eliminated at link time.
storeCgGutsForDCE :: DynFlags -> CgGuts -> ModLocation -> IO ()
storeCgGutsForDCE dflags cgguts mod_loc = do
    mctx <- readIORef globalDCEContext
    case mctx of
        Nothing -> return ()
        Just ctx | not (dce_enabled ctx) -> return ()
        Just ctx -> do
            let mod = cg_module cgguts
                isMainModule = moduleName mod == mainModuleNameIs dflags
            -- Only store for non-Main modules
            when (not isMainModule) $ do
                let binds = cg_binds cgguts
                    storedCore = StoredModuleCore
                        { smc_module   = mod
                        , smc_cgguts   = cgguts
                        , smc_location = mod_loc
                        , smc_names    = mkNameSet (concatMap bindNames binds)
                        }
                modifyIORef' (dce_module_cores ctx) (M.insert mod storedCore)
  where
    bindNames (NonRec b _) = [idName b]
    bindNames (Rec pairs) = map (idName . fst) pairs

-- -----------------------------------------------------------------------------
-- Link-time deferred codegen
--
-- These functions are used at link time to regenerate object files
-- with dead code eliminated for library modules.

-- | Get all stored module cores for deferred codegen
getStoredModuleCores :: DCEContext -> IO (M.Map Module StoredModuleCore)
getStoredModuleCores ctx = readIORef (dce_module_cores ctx)

-- | Get filtered CgGuts for a module, keeping only live bindings
--
-- This is called at link time after finalizeDCE. It returns CgGuts
-- with dead code removed, along with ModLocation, ready to be passed
-- to hscGenHardCode for recompilation.
--
-- Returns Nothing if we don't have stored CgGuts for this module.
getFilteredCgGutsForModule :: DCEContext -> Module -> IO (Maybe (CgGuts, ModLocation))
getFilteredCgGutsForModule ctx mod = do
    moduleCores <- readIORef (dce_module_cores ctx)
    case M.lookup mod moduleCores of
        Nothing -> return Nothing
        Just storedCore -> do
            liveSet <- getLiveBindings (dce_liveness ctx)
            let cgguts = smc_cgguts storedCore
                filteredBinds = filterLiveBindings liveSet (cg_binds cgguts)
                -- Return CgGuts with filtered bindings
                filteredCgGuts = cgguts { cg_binds = filteredBinds }
            return (Just (filteredCgGuts, smc_location storedCore))

-- | Get modules that have dead code (bindings that can be eliminated)
--
-- Returns a list of (Module, deadCount, totalCount) for modules
-- where deadCount > 0.
getModulesWithDeadCode :: DCEContext -> IO [(Module, Int, Int)]
getModulesWithDeadCode ctx = do
    moduleCores <- readIORef (dce_module_cores ctx)
    liveSet <- getLiveBindings (dce_liveness ctx)

    let analyzeModule :: StoredModuleCore -> Maybe (Module, Int, Int)
        analyzeModule smc =
            let allNames = smc_names smc
                allCount = sizeNameSet' allNames
                liveCount = sizeNameSet' (allNames `intersectNameSet` liveSet)
                deadCount = allCount - liveCount
            in if deadCount > 0
               then Just (smc_module smc, deadCount, allCount)
               else Nothing

    return $ mapMaybe analyzeModule (M.elems moduleCores)
  where
    mapMaybe _ [] = []
    mapMaybe f (x:xs) = case f x of
        Nothing -> mapMaybe f xs
        Just y  -> y : mapMaybe f xs

    sizeNameSet' = length . nameSetElemsStable

    -- Intersection of two NameSets
    intersectNameSet ns1 ns2 =
        filterNameSet (`elemNameSet` ns2) ns1

-- -----------------------------------------------------------------------------
-- Liveness checking for StgToCmm
--
-- Used to filter dead DataCons during code generation

-- | Check if a name is live according to DCE analysis
--
-- This is used by StgToCmm to filter dead DataCons. If DCE is disabled
-- or not initialized, returns True (assume live). Otherwise, checks
-- if the name is in the current live set.
isNameLive :: Name -> IO Bool
isNameLive name = do
    mctx <- readIORef globalDCEContext
    case mctx of
        Nothing -> do
            -- Debug: context is Nothing
            -- debugTraceMsg (text "isNameLive: context is Nothing for" <+> ppr name)
            return True  -- DCE not enabled, assume live
        Just ctx | not (dce_enabled ctx) -> do
            -- Debug: DCE not enabled
            return True
        Just ctx -> do
            -- IMPORTANT: Only filter dead code AFTER DCE is finalized
            -- Before finalization, we don't know what's truly dead yet
            finalized <- readIORef (ls_finalized (dce_liveness ctx))
            if not finalized
                then return True  -- Not finalized yet, assume live
                else do
                    liveSet <- getLiveBindings (dce_liveness ctx)
                    let isLive = name `elemNameSet` liveSet
                    return isLive
