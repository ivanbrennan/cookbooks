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
  ( (:<|>) ((:<|>)),
    (:>),
    Application,
    Get,
    Handler,
    JSON,
    ReqBody,
    Server,
    ServerT,
    hoistServer,
    serve,
  )

funToHandler :: (String -> a) -> Handler a
funToHandler f = pure (f "hi")

type ReaderAPI =
  "a" :> Get '[JSON] Int
    :<|> "b" :> ReqBody '[JSON] Double :> Get '[JSON] Bool

readerAPI :: Proxy ReaderAPI
readerAPI = Proxy

funServerT :: ServerT ReaderAPI ((->) String)
funServerT = a :<|> b
  where
    a :: String -> Int
    a _ = 1797
    b :: Double -> String -> Bool
    b _ s = s == "hi"

funServer :: Server ReaderAPI
funServer = hoistServer readerAPI funToHandler funServerT

app :: Application
app = serve readerAPI funServer

runServer :: IO ()
runServer = do
  putStrLn "Starting server"
  run 8081 app
