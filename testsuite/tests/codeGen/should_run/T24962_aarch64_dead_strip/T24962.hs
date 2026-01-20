-- Test for dead strip support on AArch64/Darwin
-- See Note [Dead strip prevention on AArch64/Darwin] in GHC.CmmToAsm.AArch64.Ppr
--
-- This test verifies that GHC-generated code works correctly when linked
-- with -dead_strip on Darwin. The key issue is that info tables must not
-- be stripped even though they appear unreferenced (they're referenced
-- via tables-next-to-code).

module Main where

-- Use a case expression to generate case continuation info tables
-- These are particularly important to test because they're internal
-- and could be incorrectly stripped.
main :: IO ()
main = do
    let xs = [1,2,3] :: [Int]
    let x = case xs of
              [] -> "empty"
              (y:ys) -> case ys of
                          [] -> "one: " ++ show y
                          (z:_) -> "multiple: " ++ show y ++ ", " ++ show z
    putStrLn x
