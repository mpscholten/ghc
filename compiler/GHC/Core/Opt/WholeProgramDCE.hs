{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- | Whole-program dead code elimination for GHC
--
-- This module implements DCE that eliminates dead code BEFORE codegen.
-- Dead bindings never get compiled to STG/Cmm/assembly, saving compile
-- time and producing smaller binaries.
--
-- The key insight is that we don't need to wait for ALL modules to know
-- that SOME things are definitely live:
--   - main is definitely live
--   - Foreign exports are definitely live
--   - Anything they directly call is definitely live
--
-- As more modules finish Core compilation, we learn more.
--
-- See Note [Whole-Program DCE Strategy] in Types.hs
module GHC.Core.Opt.WholeProgramDCE
    ( -- * Liveness state management
      initLiveness
    , moduleFinished
    , moduleFinishedWithRoots
    , addRoots
    , checkLiveness
    , finalizeLiveness
    , getLiveBindings

      -- * Building dependency information
    , coreToDepNodes
    , findInitialRoots
    , findModuleRoots

      -- * Filtering
    , filterLiveBindings
    , partitionByLiveness

      -- * Re-exports
    , LivenessState
    , LivenessResult(..)
    ) where

import GHC.Prelude

import GHC.Core
import GHC.Core.FVs (exprSomeFreeVars)
import GHC.Core.DataCon (dataConName, dataConWorkId)
import GHC.Types.Id (Id, isId, idName, isDataConWorkId_maybe)
import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Types.Unique.Set (nonDetEltsUniqSet)
import GHC.Unit.Module

import GHC.Driver.DynFlags (DynFlags, mainModuleNameIs, mainFunIs)

import GHC.Core.Opt.WholeProgramDCE.Types

import Data.IORef

-- | Initialize liveness tracking for whole-program DCE
--
-- Creates empty state - roots are added dynamically as modules are processed.
initLiveness :: DynFlags -> IO LivenessState
initLiveness _dflags = initLivenessState emptyNameSet

-- | Find the initial root names that are definitely live
--
-- Note: This returns empty because we can't know the actual Names
-- until modules are processed. Use 'addRoots' to add roots dynamically.
findInitialRoots :: DynFlags -> NameSet
findInitialRoots _dflags = emptyNameSet

-- | Add new root names to the live set
--
-- Called when we discover new roots (e.g., main function, foreign exports).
addRoots :: LivenessState -> NameSet -> IO ()
addRoots ls newRoots = do
    modifyIORef' (ls_live ls) (unionNameSet newRoots)

-- | Called when a module finishes Core optimization
--
-- This adds the module's bindings to the dependency graph and
-- propagates liveness from known roots through the new bindings.
moduleFinished :: LivenessState -> Module -> CoreProgram -> IO ()
moduleFinished ls mod core = do
    -- Convert Core bindings to dependency nodes
    let nodes = coreToDepNodes mod core

    -- Add nodes to the graph
    modifyIORef' (ls_graph ls) (addNodesToGraph nodes)

    -- Propagate liveness from known roots
    propagateLiveness ls

-- | Register a module and its roots with the liveness tracker
--
-- This is the main entry point for module processing. It:
--   1. Identifies any roots in this module (main, foreign exports)
--   2. Adds bindings to the dependency graph
--   3. Propagates liveness
moduleFinishedWithRoots :: LivenessState -> DynFlags -> Module -> CoreProgram -> IO ()
moduleFinishedWithRoots ls dflags mod core = do
    -- Find and add any roots from this module
    let roots = findModuleRoots dflags mod core
    addRoots ls roots

    -- Process the module normally
    moduleFinished ls mod core

-- | Find root bindings in a specific module
--
-- Identifies:
--   - The main function (if this is the main module)
--   - Foreign exports
findModuleRoots :: DynFlags -> Module -> CoreProgram -> NameSet
findModuleRoots dflags mod binds
    | moduleName mod == mainModuleNameIs dflags = findMainRoot dflags binds
    | otherwise = emptyNameSet
    -- TODO: Add foreign export detection

-- | Find the main function in a Core program
findMainRoot :: DynFlags -> CoreProgram -> NameSet
findMainRoot dflags binds =
    let mainFnName = case mainFunIs dflags of
            Nothing -> "main"
            Just fn -> fn
    in mkNameSet [ idName b
                 | bind <- binds
                 , b <- bindersOfBind bind
                 , getOccString (idName b) == mainFnName
                 ]

-- | Convert a Core program to dependency nodes
--
-- Each binding becomes a node with:
--   - Its name
--   - The module it's from
--   - The names of other bindings it depends on (free variables)
coreToDepNodes :: Module -> CoreProgram -> [DepNode]
coreToDepNodes mod binds = concatMap bindToNodes binds
  where
    bindToNodes :: CoreBind -> [DepNode]
    bindToNodes (NonRec b e) = [mkNode b e]
    bindToNodes (Rec pairs) =
        let recNames = mkNameSet (map (idName . fst) pairs)
        in map (\(b,e) -> mkNodeRec b e recNames) pairs

    mkNode :: Id -> CoreExpr -> DepNode
    mkNode b e = DepNode
        { dn_name   = idName b
        , dn_module = mod
        , dn_deps   = exprFreeNames e
        }

    -- For recursive bindings, exclude self-references from deps
    mkNodeRec :: Id -> CoreExpr -> NameSet -> DepNode
    mkNodeRec b e recNames = DepNode
        { dn_name   = idName b
        , dn_module = mod
        , dn_deps   = exprFreeNames e `minusNameSet` recNames
        }

    -- Get the free Names from an expression, including:
    --   - Global Ids (top-level bindings)
    --   - DataCon names from constructor applications (via worker Ids)
    --   - DataCon names from case pattern matches
    --
    -- Note: We use exprSomeFreeVars isId instead of exprFreeIds because
    -- exprFreeIds only returns locally-defined free Ids, but we need
    -- to track dependencies on global (top-level) bindings too.
    exprFreeNames :: CoreExpr -> NameSet
    exprFreeNames e =
        let fvs = exprSomeFreeVars isId e
            -- Get variable names, plus DataCon names for worker Ids
            varNames = unionNameSets
                [ case isDataConWorkId_maybe v of
                    Just dc -> mkNameSet [idName v, dataConName dc]
                    Nothing -> unitNameSet (idName v)
                | v <- nonDetEltsUniqSet fvs
                ]
            -- Also collect DataCon names from case patterns
            patternDataConNames = collectDataConNames e
        in varNames `unionNameSet` patternDataConNames

-- | Collect DataCon names from case pattern matches in an expression
--
-- This traverses the expression tree and extracts the names of all
-- DataCons used in case alternatives. This is necessary for DCE because
-- a DataCon might only be referenced in a pattern match, not as a
-- constructor application.
collectDataConNames :: CoreExpr -> NameSet
collectDataConNames = go
  where
    go (Var _)               = emptyNameSet
    go (Lit _)               = emptyNameSet
    go (Type _)              = emptyNameSet
    go (Coercion _)          = emptyNameSet
    go (App e1 e2)           = go e1 `unionNameSet` go e2
    go (Lam _ body)          = go body
    go (Let bind body)       = goBind bind `unionNameSet` go body
    go (Case scrut _ _ alts) = go scrut `unionNameSet` goAlts alts
    go (Cast e _)            = go e
    go (Tick _ e)            = go e

    goBind (NonRec _ rhs) = go rhs
    goBind (Rec pairs)    = unionNameSets (map (go . snd) pairs)

    goAlts = unionNameSets . map goAlt

    goAlt (Alt (DataAlt dc) _ rhs) =
        -- Add both DataCon name and worker Id name
        -- (they have different Uniques, so both are needed for liveness tracking)
        mkNameSet [dataConName dc, idName (dataConWorkId dc)] `unionNameSet` go rhs
    goAlt (Alt _ _ rhs) = go rhs

-- | Propagate liveness through the dependency graph
--
-- Starting from the current live set, mark all bindings reachable
-- through the dependency graph as live.
propagateLiveness :: LivenessState -> IO ()
propagateLiveness ls = do
    graph <- readIORef (ls_graph ls)
    live <- readIORef (ls_live ls)

    -- BFS to find all reachable names
    let reachable = computeReachable graph live
    writeIORef (ls_live ls) reachable

-- | Compute all names reachable from the given roots
--
-- Uses BFS to find all bindings transitively reachable from the roots.
-- A binding is reachable if it's a root OR if it's a dependency of a
-- reachable binding that exists in our graph.
computeReachable :: DepGraph -> NameSet -> NameSet
computeReachable graph roots = go emptyNameSet (nameSetElemsStable roots)
  where
    go :: NameSet -> [Name] -> NameSet
    go visited [] = visited
    go visited (n:ns)
        | n `elemNameSet` visited = go visited ns  -- Already processed
        | otherwise =
            let visited' = extendNameSet visited n
            in case lookupNode n graph of
                -- Name not in our graph (external or not yet compiled)
                -- Mark as visited but don't follow deps
                Nothing -> go visited' ns
                -- Found in graph - follow its dependencies
                Just node ->
                    let deps = nameSetElemsStable (dn_deps node)
                        newDeps = filter (not . (`elemNameSet` visited')) deps
                    in go visited' (newDeps ++ ns)

-- | Check if a binding is live, dead, or unknown
--
-- - DefinitelyLive: The binding is reachable from known roots
-- - DefinitelyDead: Analysis is finalized and binding is not live
-- - Unknown: Analysis not finalized, binding might become live
checkLiveness :: LivenessState -> Name -> IO LivenessResult
checkLiveness ls name = do
    live <- readIORef (ls_live ls)
    finalized <- readIORef (ls_finalized ls)

    return $ if name `elemNameSet` live
        then DefinitelyLive
        else if finalized
            then DefinitelyDead
            else Unknown

-- | Finalize liveness analysis
--
-- Called when all modules have been processed. After this,
-- any binding not marked live is definitely dead.
finalizeLiveness :: LivenessState -> IO NameSet
finalizeLiveness ls = do
    -- Final propagation
    propagateLiveness ls

    -- Mark as finalized
    writeIORef (ls_finalized ls) True

    -- Return the final live set
    readIORef (ls_live ls)

-- | Get all currently known live bindings
getLiveBindings :: LivenessState -> IO NameSet
getLiveBindings ls = readIORef (ls_live ls)

-- | Filter a Core program to only include live bindings
filterLiveBindings :: NameSet -> CoreProgram -> CoreProgram
filterLiveBindings liveNames = filter isLive
  where
    isLive :: CoreBind -> Bool
    isLive (NonRec b _) = idName b `elemNameSet` liveNames
    isLive (Rec pairs) = any (\(b,_) -> idName b `elemNameSet` liveNames) pairs

-- | Partition bindings by their liveness status
--
-- Returns (definitely live, unknown, definitely dead)
partitionByLiveness :: LivenessState -> CoreProgram -> IO ([CoreBind], [CoreBind], [CoreBind])
partitionByLiveness ls binds = do
    live <- readIORef (ls_live ls)
    finalized <- readIORef (ls_finalized ls)

    let classify :: CoreBind -> (Maybe CoreBind, Maybe CoreBind, Maybe CoreBind)
        classify bind =
            let names = bindersOfBind bind
                anyLive = any (\b -> idName b `elemNameSet` live) names
            in if anyLive
                then (Just bind, Nothing, Nothing)
                else if finalized
                    then (Nothing, Nothing, Just bind)
                    else (Nothing, Just bind, Nothing)

        (liveBinds, unknownBinds, deadBinds) =
            foldr (\b (ls, us, ds) ->
                    let (l, u, d) = classify b
                    in (maybe ls (:ls) l, maybe us (:us) u, maybe ds (:ds) d))
                  ([], [], [])
                  binds

    return (liveBinds, unknownBinds, deadBinds)

-- | Get the binders from a CoreBind
bindersOfBind :: CoreBind -> [Id]
bindersOfBind (NonRec b _) = [b]
bindersOfBind (Rec pairs) = map fst pairs
