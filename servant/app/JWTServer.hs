module Main where

import Cookbooks.Servant.JWT (runJWTServer)

main :: IO ()
main = runJWTServer
