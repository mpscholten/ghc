module Main where

import BigLib (usedFunc1, usedFunc2, usedFunc3)

main :: IO ()
main = do
    let x = usedFunc1 10       -- 11
    let s = usedFunc2 "hello"  -- "HELLO"  
    let n = usedFunc3 [1..10]  -- 55
    putStrLn $ "Result: " ++ show x ++ ", " ++ s ++ ", " ++ show n
