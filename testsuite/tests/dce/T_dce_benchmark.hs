-- Benchmark for whole-program DCE
-- Contains many dead functions to measure elimination effectiveness

module Main where

import Data.List (foldl')

-- LIVE: main and its dependencies
main :: IO ()
main = do
    print (liveComputation 100)
    print (anotherLiveFunction "hello")

liveComputation :: Int -> Int
liveComputation n = foldl' (+) 0 [1..n]

anotherLiveFunction :: String -> String
anotherLiveFunction s = reverse s ++ "!"

-- DEAD: None of these are reachable from main

deadFunction1 :: Int -> Int
deadFunction1 x = x * 2 + deadHelper1 x

deadHelper1 :: Int -> Int
deadHelper1 x = x * x * x

deadFunction2 :: String -> String
deadFunction2 s = concat (replicate 10 s) ++ deadHelper2

deadHelper2 :: String
deadHelper2 = "this is dead code"

deadFunction3 :: [Int] -> [Int]
deadFunction3 xs = map deadTransform xs

deadTransform :: Int -> Int
deadTransform x = x * 3 + 7

deadFunction4 :: Int -> Int -> Int
deadFunction4 a b = a * b + deadFunction1 a + deadFunction1 b

deadFunction5 :: [String] -> String
deadFunction5 = foldr (\s acc -> deadFunction2 s ++ acc) ""

deadRecursive1 :: Int -> Int
deadRecursive1 0 = 0
deadRecursive1 n = n + deadRecursive1 (n - 1)

deadRecursive2 :: Int -> Int
deadRecursive2 0 = 1
deadRecursive2 n = n * deadRecursive2 (n - 1)

deadMutualA :: Int -> Int
deadMutualA 0 = 0
deadMutualA n = deadMutualB (n - 1) + 1

deadMutualB :: Int -> Int
deadMutualB 0 = 0
deadMutualB n = deadMutualA (n - 1) + 2

-- More dead functions with complex bodies
deadComplex1 :: [Int] -> Int
deadComplex1 xs = 
    let filtered = filter (> 10) xs
        mapped = map (* 2) filtered
        summed = sum mapped
    in summed + length xs

deadComplex2 :: [(Int, String)] -> [(String, Int)]
deadComplex2 = map (\(i, s) -> (s ++ show i, i * 2))

deadComplex3 :: Int -> [Int]
deadComplex3 n = take n (iterate deadTransform 1)

deadWithGuards :: Int -> String
deadWithGuards x
    | x < 0     = "negative"
    | x == 0    = "zero"
    | x < 10    = "small"
    | x < 100   = "medium"
    | otherwise = "large"

deadWithCase :: Maybe Int -> Int
deadWithCase Nothing = 0
deadWithCase (Just x) = x * 2

deadHigherOrder :: (Int -> Int) -> [Int] -> [Int]
deadHigherOrder f = map f . filter (> 0)

deadPartialApp :: Int -> Int
deadPartialApp = deadFunction4 42

deadComposition :: String -> String
deadComposition = reverse . deadFunction2 . reverse

-- Even more dead code
deadList1, deadList2, deadList3 :: [Int]
deadList1 = [1..100]
deadList2 = map (* 2) deadList1
deadList3 = filter odd deadList2

deadTuple :: (Int, Int, Int)
deadTuple = (deadRecursive1 10, deadRecursive2 5, deadMutualA 7)

deadString :: String
deadString = deadFunction2 "benchmark" ++ show deadTuple
