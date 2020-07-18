module Main where

import Cookbooks.Servant.Pagination (runPaginatedServer)

main :: IO ()
main = runPaginatedServer
