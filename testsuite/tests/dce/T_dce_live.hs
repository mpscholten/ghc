-- | Test that live functions are preserved through DCE
-- All functions used by main should be preserved
module Main where

helper1 :: Int -> Int
helper1 x = x * 2

helper2 :: Int -> Int
helper2 x = helper1 x + 1

helper3 :: Int -> Int
helper3 x = helper2 (helper1 x)

-- Dead helper - not used
deadHelper :: Int -> Int
deadHelper x = x - 1

main :: IO ()
main = do
    let result = helper3 10
    print result
