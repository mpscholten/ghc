-- A larger library module with lots of dead code
-- to test DCE performance benefits
module BigLib where

import Data.List (sort, foldl')
import Data.Char (toUpper, toLower)

-- ============================================
-- USED FUNCTIONS (will be kept by DCE)
-- ============================================

usedFunc1 :: Int -> Int
usedFunc1 x = x + 1

usedFunc2 :: String -> String  
usedFunc2 = map toUpper

usedFunc3 :: [Int] -> Int
usedFunc3 = foldl' (+) 0

-- ============================================
-- DEAD CODE - Complex functions that won't be used
-- These represent typical library code that goes unused
-- ============================================

-- Dead sorting utilities
deadQuickSort :: Ord a => [a] -> [a]
deadQuickSort [] = []
deadQuickSort (x:xs) = deadQuickSort [y | y <- xs, y < x] ++ [x] ++ deadQuickSort [y | y <- xs, y >= x]

deadMergeSort :: Ord a => [a] -> [a]
deadMergeSort [] = []
deadMergeSort [x] = [x]
deadMergeSort xs = deadMerge (deadMergeSort left) (deadMergeSort right)
  where
    (left, right) = splitAt (length xs `div` 2) xs

deadMerge :: Ord a => [a] -> [a] -> [a]
deadMerge [] ys = ys
deadMerge xs [] = xs
deadMerge (x:xs) (y:ys)
    | x <= y    = x : deadMerge xs (y:ys)
    | otherwise = y : deadMerge (x:xs) ys

deadInsertionSort :: Ord a => [a] -> [a]
deadInsertionSort = foldr deadInsert []

deadInsert :: Ord a => a -> [a] -> [a]
deadInsert x [] = [x]
deadInsert x (y:ys)
    | x <= y    = x : y : ys
    | otherwise = y : deadInsert x ys

deadBubbleSort :: Ord a => [a] -> [a]
deadBubbleSort xs = if swapped then deadBubbleSort xs' else xs'
  where
    (xs', swapped) = deadBubblePass xs

deadBubblePass :: Ord a => [a] -> ([a], Bool)
deadBubblePass [] = ([], False)
deadBubblePass [x] = ([x], False)
deadBubblePass (x:y:rest)
    | x > y     = let (rest', _) = deadBubblePass (x:rest) in (y:rest', True)
    | otherwise = let (rest', s) = deadBubblePass (y:rest) in (x:rest', s)

-- Dead tree operations
data DeadTree a = DeadLeaf | DeadNode a (DeadTree a) (DeadTree a)
    deriving (Show, Eq)

deadTreeInsert :: Ord a => a -> DeadTree a -> DeadTree a
deadTreeInsert x DeadLeaf = DeadNode x DeadLeaf DeadLeaf
deadTreeInsert x (DeadNode y left right)
    | x < y     = DeadNode y (deadTreeInsert x left) right
    | x > y     = DeadNode y left (deadTreeInsert x right)
    | otherwise = DeadNode y left right

deadTreeSearch :: Ord a => a -> DeadTree a -> Bool
deadTreeSearch _ DeadLeaf = False
deadTreeSearch x (DeadNode y left right)
    | x < y     = deadTreeSearch x left
    | x > y     = deadTreeSearch x right
    | otherwise = True

deadTreeToList :: DeadTree a -> [a]
deadTreeToList DeadLeaf = []
deadTreeToList (DeadNode x left right) = deadTreeToList left ++ [x] ++ deadTreeToList right

deadTreeFromList :: Ord a => [a] -> DeadTree a
deadTreeFromList = foldr deadTreeInsert DeadLeaf

deadTreeHeight :: DeadTree a -> Int
deadTreeHeight DeadLeaf = 0
deadTreeHeight (DeadNode _ left right) = 1 + max (deadTreeHeight left) (deadTreeHeight right)

deadTreeSize :: DeadTree a -> Int
deadTreeSize DeadLeaf = 0
deadTreeSize (DeadNode _ left right) = 1 + deadTreeSize left + deadTreeSize right

-- Dead string operations
deadReverse :: [a] -> [a]
deadReverse = foldl (flip (:)) []

deadIntersperse :: a -> [a] -> [a]
deadIntersperse _ [] = []
deadIntersperse _ [x] = [x]
deadIntersperse sep (x:xs) = x : sep : deadIntersperse sep xs

deadIntercalate :: [a] -> [[a]] -> [a]
deadIntercalate sep = concat . deadIntersperse sep

deadTranspose :: [[a]] -> [[a]]
deadTranspose [] = []
deadTranspose ([] : xss) = deadTranspose xss
deadTranspose ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : deadTranspose (xs : [t | (_:t) <- xss])

deadSubsequences :: [a] -> [[a]]
deadSubsequences [] = [[]]
deadSubsequences (x:xs) = deadSubsequences xs ++ map (x:) (deadSubsequences xs)

deadPermutations :: [a] -> [[a]]
deadPermutations [] = [[]]
deadPermutations (x:xs) = concatMap (deadInsertEverywhere x) (deadPermutations xs)

deadInsertEverywhere :: a -> [a] -> [[a]]
deadInsertEverywhere x [] = [[x]]
deadInsertEverywhere x (y:ys) = (x:y:ys) : map (y:) (deadInsertEverywhere x ys)

-- Dead numeric operations
deadFactorial :: Integer -> Integer
deadFactorial 0 = 1
deadFactorial n = n * deadFactorial (n - 1)

deadFibonacci :: Int -> Integer
deadFibonacci 0 = 0
deadFibonacci 1 = 1
deadFibonacci n = deadFibonacci (n-1) + deadFibonacci (n-2)

deadFibonacciMemo :: Int -> Integer
deadFibonacciMemo n = fibs !! n
  where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

deadIsPrime :: Integer -> Bool
deadIsPrime n
    | n < 2     = False
    | n == 2    = True
    | even n    = False
    | otherwise = all (\x -> n `mod` x /= 0) [3,5..deadIsqrt n]

deadIsqrt :: Integer -> Integer
deadIsqrt = floor . sqrt . fromIntegral

deadPrimes :: [Integer]
deadPrimes = 2 : filter deadIsPrime [3,5..]

deadGcd :: Integer -> Integer -> Integer
deadGcd a 0 = abs a
deadGcd a b = deadGcd b (a `mod` b)

deadLcm :: Integer -> Integer -> Integer
deadLcm a b = abs (a * b) `div` deadGcd a b

deadPow :: Integer -> Int -> Integer
deadPow _ 0 = 1
deadPow x n
    | even n    = deadPow (x * x) (n `div` 2)
    | otherwise = x * deadPow x (n - 1)

-- Dead matrix operations
type DeadMatrix = [[Double]]

deadMatrixAdd :: DeadMatrix -> DeadMatrix -> DeadMatrix
deadMatrixAdd = zipWith (zipWith (+))

deadMatrixMult :: DeadMatrix -> DeadMatrix -> DeadMatrix
deadMatrixMult a b = [[sum $ zipWith (*) ar bc | bc <- deadTransposeD b] | ar <- a]

deadTransposeD :: DeadMatrix -> DeadMatrix
deadTransposeD ([]:_) = []
deadTransposeD m = map head m : deadTransposeD (map tail m)

deadMatrixScale :: Double -> DeadMatrix -> DeadMatrix
deadMatrixScale k = map (map (*k))

deadIdentityMatrix :: Int -> DeadMatrix
deadIdentityMatrix n = [[if i == j then 1 else 0 | j <- [0..n-1]] | i <- [0..n-1]]

deadDeterminant :: DeadMatrix -> Double
deadDeterminant [[x]] = x
deadDeterminant m = sum [(-1)^i * (head m !! i) * deadDeterminant (deadMinor m 0 i) | i <- [0..n-1]]
  where n = length m

deadMinor :: DeadMatrix -> Int -> Int -> DeadMatrix
deadMinor m i j = [[m !! r !! c | c <- [0..n-1], c /= j] | r <- [0..n-1], r /= i]
  where n = length m

-- More dead functions to increase the amount of dead code
deadMap :: (a -> b) -> [a] -> [b]
deadMap _ [] = []
deadMap f (x:xs) = f x : deadMap f xs

deadFilter :: (a -> Bool) -> [a] -> [a]
deadFilter _ [] = []
deadFilter p (x:xs)
    | p x       = x : deadFilter p xs
    | otherwise = deadFilter p xs

deadFoldr :: (a -> b -> b) -> b -> [a] -> b
deadFoldr _ z [] = z
deadFoldr f z (x:xs) = f x (deadFoldr f z xs)

deadFoldl :: (b -> a -> b) -> b -> [a] -> b
deadFoldl _ z [] = z
deadFoldl f z (x:xs) = deadFoldl f (f z x) xs

deadZip :: [a] -> [b] -> [(a, b)]
deadZip [] _ = []
deadZip _ [] = []
deadZip (x:xs) (y:ys) = (x, y) : deadZip xs ys

deadZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
deadZipWith _ [] _ = []
deadZipWith _ _ [] = []
deadZipWith f (x:xs) (y:ys) = f x y : deadZipWith f xs ys

deadTake :: Int -> [a] -> [a]
deadTake 0 _ = []
deadTake _ [] = []
deadTake n (x:xs) = x : deadTake (n-1) xs

deadDrop :: Int -> [a] -> [a]
deadDrop 0 xs = xs
deadDrop _ [] = []
deadDrop n (_:xs) = deadDrop (n-1) xs

deadSplitAt :: Int -> [a] -> ([a], [a])
deadSplitAt n xs = (deadTake n xs, deadDrop n xs)

deadTakeWhile :: (a -> Bool) -> [a] -> [a]
deadTakeWhile _ [] = []
deadTakeWhile p (x:xs)
    | p x       = x : deadTakeWhile p xs
    | otherwise = []

deadDropWhile :: (a -> Bool) -> [a] -> [a]
deadDropWhile _ [] = []
deadDropWhile p (x:xs)
    | p x       = deadDropWhile p xs
    | otherwise = x : xs

deadSpan :: (a -> Bool) -> [a] -> ([a], [a])
deadSpan p xs = (deadTakeWhile p xs, deadDropWhile p xs)

deadBreak :: (a -> Bool) -> [a] -> ([a], [a])
deadBreak p = deadSpan (not . p)

deadElem :: Eq a => a -> [a] -> Bool
deadElem _ [] = False
deadElem x (y:ys) = x == y || deadElem x ys

deadNotElem :: Eq a => a -> [a] -> Bool
deadNotElem x = not . deadElem x

deadLookup :: Eq a => a -> [(a, b)] -> Maybe b
deadLookup _ [] = Nothing
deadLookup k ((x,y):xys)
    | k == x    = Just y
    | otherwise = deadLookup k xys

deadConcat :: [[a]] -> [a]
deadConcat = deadFoldr (++) []

deadConcatMap :: (a -> [b]) -> [a] -> [b]
deadConcatMap f = deadConcat . deadMap f

deadAnd :: [Bool] -> Bool
deadAnd = deadFoldr (&&) True

deadOr :: [Bool] -> Bool
deadOr = deadFoldr (||) False

deadAny :: (a -> Bool) -> [a] -> Bool
deadAny p = deadOr . deadMap p

deadAll :: (a -> Bool) -> [a] -> Bool
deadAll p = deadAnd . deadMap p

deadSum :: Num a => [a] -> a
deadSum = deadFoldl (+) 0

deadProduct :: Num a => [a] -> a
deadProduct = deadFoldl (*) 1

deadMaximum :: Ord a => [a] -> a
deadMaximum [x] = x
deadMaximum (x:xs) = max x (deadMaximum xs)
deadMaximum [] = error "empty list"

deadMinimum :: Ord a => [a] -> a
deadMinimum [x] = x
deadMinimum (x:xs) = min x (deadMinimum xs)
deadMinimum [] = error "empty list"

deadLength :: [a] -> Int
deadLength = deadFoldl (\n _ -> n + 1) 0

deadNull :: [a] -> Bool
deadNull [] = True
deadNull _ = False

deadHead :: [a] -> a
deadHead (x:_) = x
deadHead [] = error "empty list"

deadTail :: [a] -> [a]
deadTail (_:xs) = xs
deadTail [] = error "empty list"

deadInit :: [a] -> [a]
deadInit [_] = []
deadInit (x:xs) = x : deadInit xs
deadInit [] = error "empty list"

deadLast :: [a] -> a
deadLast [x] = x
deadLast (_:xs) = deadLast xs
deadLast [] = error "empty list"
