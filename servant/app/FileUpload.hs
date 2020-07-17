module Main where

import Cookbooks.Servant.FileUpload (runFileUploadServer)

main :: IO ()
main = runFileUploadServer
