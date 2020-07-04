{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Cookbooks.Servant
  ( runServer,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant ((:>), Application, Get, JSON, Server, err404, errBody, serve, throwError)
import System.Directory (doesFileExist)

type IOAPI1 = "myfile.txt" :> Get '[JSON] FileContent

newtype FileContent
  = FileContent
      {content :: String}
  deriving (Generic)

instance ToJSON FileContent

server6 :: Server IOAPI1
server6 = do
  exists <- liftIO (doesFileExist "myfile.txt")
  if exists
    then FileContent <$> liftIO (readFile "myfile.txt")
    else throwError custom404Err
  where
    custom404Err = err404 {errBody = "myfile.txt just isn't there."}

ioAPI :: Proxy IOAPI1
ioAPI = Proxy

app6 :: Application
app6 = serve ioAPI server6

runServer :: IO ()
runServer = do
  putStrLn "Starting server"
  run 8081 app6
