module Main where

import Cookbooks.Servant.Javascript (runJavascriptServer)

main :: IO ()
main = putStrLn "Starting Javascript server" >> runJavascriptServer
