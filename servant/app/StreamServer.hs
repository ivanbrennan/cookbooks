module Main where

import Cookbooks.Servant.Server (runStreamServer)

main :: IO ()
main = putStrLn "Starting stream server" >> runStreamServer
