main :: IO ()
main = do
  let xs = [1..100] :: [Int]
  putStrLn (show (sum xs))
