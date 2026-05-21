-- | Test DCE with typeclass instances
-- Used instances should be preserved, unused ones could be eliminated
module Main where

class MyClass a where
    myMethod :: a -> Int

-- This instance is used
instance MyClass Int where
    myMethod x = x + 1

-- This instance is not used
instance MyClass Bool where
    myMethod True = 1
    myMethod False = 0

-- Another unused instance
instance MyClass Char where
    myMethod _ = 42

main :: IO ()
main = print (myMethod (5 :: Int))
