module Main where

import Cookbooks.Servant.Generics (cliGet)

main :: IO ()
main = cliGet 1 >>= print
