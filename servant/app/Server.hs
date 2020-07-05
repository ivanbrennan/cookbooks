module Main where

import Cookbooks.Servant.Server (runServer)

main :: IO ()
main = putStrLn "Starting server" >> runServer
