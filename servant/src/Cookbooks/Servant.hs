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
import Servant ((:>), Application, Get, JSON, Server, serve)

type IOAPI1 = "myfile.txt" :> Get '[JSON] FileContent

newtype FileContent
  = FileContent
      {content :: String}
  deriving (Generic)

instance ToJSON FileContent

server5 :: Server IOAPI1
server5 = do
  filecontent <- liftIO (readFile "myfile.txt")
  pure (FileContent filecontent)

ioAPI :: Proxy IOAPI1
ioAPI = Proxy

app5 :: Application
app5 = serve ioAPI server5

runServer :: IO ()
runServer = do
  putStrLn "Starting server"
  run 8081 app5
