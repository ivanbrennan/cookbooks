{-# LANGUAGE DataKinds #-}

module Cookbooks.Servant.Https
  ( runHttpsServer,
  )
where

import Data.Proxy (Proxy (Proxy))
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Servant (Application, Get, JSON, Server, serve)

type API = Get '[JSON] Int

api :: Proxy API
api = Proxy

server :: Server API
server = pure 10

app :: Application
app = serve api server

runHttpsServer :: IO ()
runHttpsServer = runTLS tlsOpts warpOpts app
  where
    tlsOpts = tlsSettings "certificate.pem" "key.pem"
    warpOpts = setPort 8080 defaultSettings
