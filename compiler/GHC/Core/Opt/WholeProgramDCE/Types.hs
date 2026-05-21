{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Types for whole-program dead code elimination
--
-- This module defines the data types used for tracking liveness of bindings
-- across the whole program during compilation. The key insight is that we can
-- determine some bindings are "definitely live" (reachable from roots like main)
-- without waiting for all modules to be compiled.
--
-- See Note [Whole-Program DCE Strategy] for the overall approach.
module GHC.Core.Opt.WholeProgramDCE.Types
    ( -- * Liveness tracking
      LivenessState(..)
    , LivenessResult(..)
    , initLivenessState
    , emptyLivenessState

      -- * Dependency graph
    , DepGraph(..)
    , DepNode(..)
    , emptyDepGraph
    , addNodesToGraph
    , lookupNode

      -- * Deferred bindings
    , DeferredBindings(..)
    , emptyDeferredBindings

      -- * Stored module Core for deferred codegen
    , StoredModuleCore(..)
    ) where

import GHC.Prelude

import GHC.Core
import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Unit.Module
import GHC.Unit.Module.ModGuts (CgGuts(..))
import GHC.Unit.Module.Location (ModLocation(..))

import Data.IORef
import qualified Data.Map.Strict as M

{-
Note [Whole-Program DCE Strategy]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The whole-program DCE uses an incremental liveness analysis that allows
codegen to proceed speculatively while the analysis runs.

The key states for a binding are:
  1. DefinitelyLive - Reachable from known roots (main, foreign exports)
     These bindings can be compiled immediately.

  2. Unknown - Might be live depending on modules not yet compiled.
     These bindings wait until more information is available.

  3. DefinitelyDead - Not reachable from any root after all modules done.
     These bindings are skipped entirely - never compiled!

The algorithm works as follows:
  - Initialize with root bindings (main, foreign exports)
  - As each module finishes Core optimization, add its bindings to the graph
  - Propagate liveness from known-live bindings through dependencies
  - After all modules are done, anything not marked live is dead

This allows:
  - Starting codegen immediately for definitely-live code
  - Running analysis in parallel with compilation
  - Skipping codegen entirely for dead code
-}

-- | The result of checking a binding's liveness status
data LivenessResult
    = DefinitelyLive    -- ^ Reachable from known roots - compile now
    | DefinitelyDead    -- ^ Not reachable from any root - skip codegen
    | Unknown           -- ^ Might be live - wait for more info
    deriving (Eq, Show)

-- | A node in the dependency graph representing a binding
data DepNode = DepNode
    { dn_name    :: !Name          -- ^ The binding's name
    , dn_module  :: !Module        -- ^ Module containing this binding
    , dn_deps    :: !NameSet       -- ^ Names this binding depends on
    }

instance Eq DepNode where
    a == b = dn_name a == dn_name b

instance Ord DepNode where
    compare a b = compare (dn_name a) (dn_name b)

-- | The dependency graph tracks all bindings and their dependencies
data DepGraph = DepGraph
    { dg_nodes       :: !(M.Map Name DepNode)  -- ^ All nodes by name
    , dg_reverse     :: !(M.Map Name NameSet)  -- ^ Reverse edges: who depends on me
    }

-- | Create an empty dependency graph
emptyDepGraph :: DepGraph
emptyDepGraph = DepGraph
    { dg_nodes   = M.empty
    , dg_reverse = M.empty
    }

-- | Add nodes to the dependency graph
addNodesToGraph :: [DepNode] -> DepGraph -> DepGraph
addNodesToGraph nodes graph = foldr addNode graph nodes
  where
    addNode node DepGraph{..} = DepGraph
        { dg_nodes   = M.insert (dn_name node) node dg_nodes
        , dg_reverse = foldr (addReverseEdge (dn_name node))
                             dg_reverse
                             (nameSetElemsStable (dn_deps node))
        }

    addReverseEdge :: Name -> Name -> M.Map Name NameSet -> M.Map Name NameSet
    addReverseEdge from to revMap =
        M.insertWith unionNameSet to (unitNameSet from) revMap

-- | Look up a node in the graph
lookupNode :: Name -> DepGraph -> Maybe DepNode
lookupNode name DepGraph{..} = M.lookup name dg_nodes

-- | Mutable state for incremental liveness tracking
--
-- This is the central data structure for DCE. It tracks:
--   - The growing dependency graph as modules are compiled
--   - The set of definitely-live bindings
--   - Bindings waiting for liveness determination
--   - The initial root bindings (main, foreign exports)
data LivenessState = LivenessState
    { ls_graph     :: !(IORef DepGraph)     -- ^ Growing dependency graph
    , ls_live      :: !(IORef NameSet)      -- ^ Definitely live bindings
    , ls_pending   :: !(IORef [Name])       -- ^ Bindings waiting for liveness check
    , ls_roots     :: !NameSet              -- ^ Initial roots (main, foreign exports)
    , ls_finalized :: !(IORef Bool)         -- ^ True after all modules done
    }

-- | Create an empty liveness state (for when DCE is disabled)
emptyLivenessState :: IO LivenessState
emptyLivenessState = do
    graphRef <- newIORef emptyDepGraph
    liveRef <- newIORef emptyNameSet
    pendingRef <- newIORef []
    finalizedRef <- newIORef False
    return LivenessState
        { ls_graph     = graphRef
        , ls_live      = liveRef
        , ls_pending   = pendingRef
        , ls_roots     = emptyNameSet
        , ls_finalized = finalizedRef
        }

-- | Initialize liveness state with the given root names
initLivenessState :: NameSet -> IO LivenessState
initLivenessState roots = do
    graphRef <- newIORef emptyDepGraph
    liveRef <- newIORef roots  -- Roots are initially live
    pendingRef <- newIORef []
    finalizedRef <- newIORef False
    return LivenessState
        { ls_graph     = graphRef
        , ls_live      = liveRef
        , ls_pending   = pendingRef
        , ls_roots     = roots
        , ls_finalized = finalizedRef
        }

-- | Bindings that have been deferred pending liveness analysis
--
-- When a module finishes Core optimization but some of its bindings
-- have Unknown liveness, we store them here for later codegen.
data DeferredBindings = DeferredBindings
    { db_module   :: !Module                -- ^ The module these bindings are from
    , db_bindings :: ![CoreBind]            -- ^ The actual Core bindings
    , db_names    :: !NameSet               -- ^ Names of the bindings (for fast lookup)
    }

-- | Empty deferred bindings
emptyDeferredBindings :: Module -> DeferredBindings
emptyDeferredBindings mod = DeferredBindings
    { db_module   = mod
    , db_bindings = []
    , db_names    = emptyNameSet
    }

-- | Stored CgGuts for a module, used for deferred codegen
--
-- When DCE is enabled, we store the CgGuts for non-Main modules
-- so that at link time we can regenerate object files with dead code
-- eliminated. CgGuts contains everything needed for codegen.
data StoredModuleCore = StoredModuleCore
    { smc_module   :: !Module         -- ^ The module
    , smc_cgguts   :: !CgGuts         -- ^ Full CgGuts for codegen
    , smc_location :: !ModLocation    -- ^ Module location (for output path)
    , smc_names    :: !NameSet        -- ^ All binding names (for fast lookup)
    }
