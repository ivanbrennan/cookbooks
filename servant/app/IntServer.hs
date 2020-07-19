module Main where

import Cookbooks.Servant.Server (runIntServer)

main :: IO ()
main = putStrLn "Starting int server" >> runIntServer
