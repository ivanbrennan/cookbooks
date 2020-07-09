module Main where

import Cookbooks.Servant.Authentication (genAuthMain)

main :: IO ()
main = putStrLn "Starting gen auth server" >> genAuthMain
