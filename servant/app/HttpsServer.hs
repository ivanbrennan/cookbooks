module Main where

import Cookbooks.Servant.Https (runHttpsServer)

main :: IO ()
main = putStrLn "Starting https server" >> runHttpsServer
