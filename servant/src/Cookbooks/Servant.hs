{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Cookbooks.Servant
  ( runServer,
  )
where

import Control.Monad.Trans.Reader (Reader, asks, runReader)
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

readerToHandler :: Reader String a -> Handler a
readerToHandler r = pure (runReader r "hi")

type ReaderAPI =
  "a" :> Get '[JSON] Int
    :<|> "b" :> ReqBody '[JSON] Double :> Get '[JSON] Bool

readerAPI :: Proxy ReaderAPI
readerAPI = Proxy

readerServerT :: ServerT ReaderAPI (Reader String)
readerServerT = a :<|> b
  where
    a :: Reader String Int
    a = pure 1797
    b :: Double -> Reader String Bool
    b _ = asks (== "hi")

readerServer :: Server ReaderAPI
readerServer = hoistServer readerAPI readerToHandler readerServerT

app :: Application
app = serve readerAPI readerServer

runServer :: IO ()
runServer = do
  putStrLn "Starting server"
  run 8081 app
