module Main (main) where
import Lib (add)

main :: IO ()
main = do
  let x = 5
      y = 10
      result = add x y
  putStrLn $ "The sum of " ++ show x ++ " and " ++ show y ++ " is " ++ show result

