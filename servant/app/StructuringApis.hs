module Main where

import Cookbooks.Servant.StructuringApis (runServer)

main :: IO ()
main = putStrLn "Starting server" >> runServer
