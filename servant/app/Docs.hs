module Main where

import Cookbooks.Servant.Docs (runDocs)

main :: IO ()
main = putStrLn "Serving docs" >> runDocs
