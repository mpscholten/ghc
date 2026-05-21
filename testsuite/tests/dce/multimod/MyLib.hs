module MyLib where

-- Functions that WILL be used
usedFunc1 :: Int -> Int
usedFunc1 x = x + 1

usedFunc2 :: Int -> Int  
usedFunc2 x = x * 2

usedFunc3 :: String -> String
usedFunc3 s = reverse s

-- Functions that WON'T be used (dead code)
deadFunc1 :: Int -> Int
deadFunc1 x = x * x + deadHelper1 x

deadHelper1 :: Int -> Int
deadHelper1 x = x * 3

deadFunc2 :: String -> String
deadFunc2 s = s ++ s ++ deadHelper2

deadHelper2 :: String
deadHelper2 = "dead"

deadFunc3 :: [Int] -> Int
deadFunc3 = foldr (+) 0 . map deadFunc1

deadFunc4 :: Int -> Int -> Int
deadFunc4 a b = deadFunc1 a + deadFunc1 b

deadFunc5 :: String -> String -> String
deadFunc5 a b = deadFunc2 a ++ deadFunc2 b

deadRecursive :: Int -> Int
deadRecursive 0 = 0
deadRecursive n = n + deadRecursive (n-1)

deadMutualA :: Int -> Int
deadMutualA 0 = 0
deadMutualA n = 1 + deadMutualB (n-1)

deadMutualB :: Int -> Int
deadMutualB 0 = 0
deadMutualB n = 2 + deadMutualA (n-1)

-- More dead functions
deadComplex1 :: [Int] -> [Int]
deadComplex1 = map (* 2) . filter (> 0)

deadComplex2 :: [(Int, String)] -> [String]
deadComplex2 = map snd . filter ((> 0) . fst)

deadWithLet :: Int -> Int
deadWithLet x = 
    let a = x * 2
        b = a + 3
        c = b * b
    in c + deadFunc1 a
