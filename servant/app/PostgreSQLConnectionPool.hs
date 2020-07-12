module Main where

import Cookbooks.Servant.PostgreSQLConnectionPool (runPostgreSQLConnectionPool)

main :: IO ()
main = runPostgreSQLConnectionPool
