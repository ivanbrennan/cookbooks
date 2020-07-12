module Main where

import Cookbooks.Servant.STM (runSTMServer)

main :: IO ()
main = runSTMServer
