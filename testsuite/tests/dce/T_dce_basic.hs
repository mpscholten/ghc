-- | Test basic dead code elimination
-- The deadFunction should be identified as dead code
-- and not cause any issues during compilation.
module Main where

liveFunction :: Int -> Int
liveFunction x = x + 1

-- This function is never called - DCE should identify it as dead
deadFunction :: Int -> Int
deadFunction x = expensiveComputation x
  where
    expensiveComputation n = n * n * n + n * n + n

-- Another dead function
anotherDeadFunction :: String -> String
anotherDeadFunction s = reverse s ++ s

main :: IO ()
main = print (liveFunction 42)
