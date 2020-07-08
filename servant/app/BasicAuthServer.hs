module Main where

import Cookbooks.Servant.Authentication (basicAuthMain)

main :: IO ()
main = putStrLn "Starting basic auth server" >> basicAuthMain
