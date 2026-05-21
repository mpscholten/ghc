-- Real-world DCE test: Import many modules, use few functions
module Main where

import Data.List (sort, foldl')
import Data.Maybe (fromMaybe)
import Data.Char (toUpper)
import Control.Monad (when, forM_)
import Data.IORef (newIORef, readIORef, modifyIORef')
import System.IO (hFlush, stdout)

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.IntMap.Strict as IM
import qualified Data.Sequence as Seq

main :: IO ()
main = do
    -- Use Data.List: sort, foldl'
    let numbers :: [Int]
        numbers = [5, 2, 8, 1, 9, 3, 7, 4, 6]
    let sorted = sort numbers
    let total = foldl' (+) 0 sorted
    putStrLn $ "Sorted: " ++ show sorted ++ ", Sum: " ++ show total
    
    -- Use Data.Map: fromList, lookup, insert
    let myMap :: M.Map String Int
        myMap = M.fromList [("alice", 30), ("bob", 25), ("charlie", 35)]
    let age = fromMaybe 0 (M.lookup "alice" myMap)
    let myMap' = M.insert "dave" 28 myMap
    putStrLn $ "Alice's age: " ++ show age ++ ", Map size: " ++ show (M.size myMap')
    
    -- Use Data.Set: fromList, member, size
    let mySet :: S.Set Int
        mySet = S.fromList [1, 2, 3, 4, 5]
    let hasThree = S.member 3 mySet
    putStrLn $ "Set has 3: " ++ show hasThree ++ ", Set size: " ++ show (S.size mySet)
    
    -- Use Data.Char: toUpper
    let greeting = map toUpper "hello world"
    putStrLn $ "Greeting: " ++ greeting
    
    -- Use Control.Monad: when
    when (total > 40) $ putStrLn "Total is greater than 40!"
    
    -- Use Data.IORef
    counter <- newIORef (0 :: Int)
    forM_ [1..5 :: Int] $ \i -> modifyIORef' counter (+i)
    finalCount <- readIORef counter
    putStrLn $ "Counter: " ++ show finalCount
    
    -- Use Data.IntMap: fromList, lookup
    let intMap :: IM.IntMap String
        intMap = IM.fromList [(1, "one"), (2, "two"), (3, "three")]
    let val = fromMaybe "unknown" (IM.lookup 2 intMap)
    putStrLn $ "IntMap lookup: " ++ val
    
    -- Use Data.Sequence: fromList, length
    let mySeq :: Seq.Seq Int
        mySeq = Seq.fromList [1..10]
    putStrLn $ "Sequence length: " ++ show (Seq.length mySeq)
    
    hFlush stdout
    putStrLn "Done!"

-- Dead local functions
unusedHelper1 :: Int -> Int
unusedHelper1 x = x * 2 + unusedHelper2 x

unusedHelper2 :: Int -> Int
unusedHelper2 x = x * x

unusedComplex :: M.Map String Int -> S.Set Int -> Int
unusedComplex m s = 
    let keys = M.keys m
        vals = M.elems m
        combined = S.union s (S.fromList vals)
    in S.size combined + length keys
