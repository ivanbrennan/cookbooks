{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Cookbooks.Servant
  ( runServer,
  )
where

import Data.Proxy (Proxy (Proxy))
import Network.Wai.Handler.Warp (run)
import Servant
  ( (:>),
    Application,
    Raw,
    Server,
    serve,
    serveDirectoryWebApp,
  )

type StaticAPI = "static" :> Raw

server7 :: Server StaticAPI
server7 = serveDirectoryWebApp "static-files"

staticAPI :: Proxy StaticAPI
staticAPI = Proxy

app7 :: Application
app7 = serve staticAPI server7

runServer :: IO ()
runServer = do
  putStrLn "Starting server"
  run 8081 app7
