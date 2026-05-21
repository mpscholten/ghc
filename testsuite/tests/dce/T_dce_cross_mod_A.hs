-- | Helper module for cross-module DCE test
module T_dce_cross_mod_A where

-- This function is used by main
liveExport :: Int -> Int
liveExport x = x * 3

-- This function is exported but never used
deadExport :: Int -> Int
deadExport x = x + 100

-- Internal helper used by liveExport (transitively live)
internalHelper :: Int -> Int
internalHelper = (* 2)

-- Internal helper not used (dead)
unusedInternal :: Int -> Int
unusedInternal = (+ 50)
