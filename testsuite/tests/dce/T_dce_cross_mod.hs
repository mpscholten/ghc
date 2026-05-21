-- | Test cross-module dead code elimination
module Main where

import T_dce_cross_mod_A (liveExport)

main :: IO ()
main = print (liveExport 7)
