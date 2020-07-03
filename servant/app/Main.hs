module Main where

import qualified Cookbooks.Servant (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Cookbooks.Servant.someFunc
