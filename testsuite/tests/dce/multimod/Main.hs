module Main where

import MyLib (usedFunc1, usedFunc2, usedFunc3)

main :: IO ()
main = do
    let x = usedFunc1 10      -- 11
    let y = usedFunc2 x       -- 22
    let s = usedFunc3 "hello" -- "olleh"
    putStrLn $ "Result: " ++ show y ++ ", " ++ s
