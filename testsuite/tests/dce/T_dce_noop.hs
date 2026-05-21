-- | Test that normal compilation without DCE still works
-- This ensures we haven't broken anything
module Main where

someFunction :: Int -> Int
someFunction x = x * 2 + 1

main :: IO ()
main = print (someFunction 20)
