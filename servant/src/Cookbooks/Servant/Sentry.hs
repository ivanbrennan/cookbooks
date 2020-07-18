{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Cookbooks.Servant.Sentry
  ( runSentry,
  )
where

import Control.Exception (Exception, SomeException, throw)
import Data.ByteString.Char8 (unpack)
import Data.Proxy (Proxy (Proxy))
import Network.Wai (Request, rawPathInfo, requestHeaderHost)
import Network.Wai.Handler.Warp
  ( defaultOnException,
    defaultSettings,
    runSettings,
    setOnException,
    setPort,
  )
import Servant ((:>), Get, Handler, JSON, serve)
import System.Log.Raven (initRaven, register, silentFallback)
import System.Log.Raven.Transport.HttpConduit (sendRecord)
import System.Log.Raven.Types (SentryLevel (Error), SentryRecord, srCulprit, srServerName)

type API = "break" :> Get '[JSON] ()

data MyException = MyException deriving (Show)

instance Exception MyException

server :: Handler ()
server = breakHandler
  where
    breakHandler :: Handler ()
    breakHandler = do
      _ <- throw MyException
      pure ()

sentryOnException :: Maybe Request -> SomeException -> IO ()
sentryOnException mRequest exception = do
  sentryService <-
    initRaven
      "https://username:password@sentry.host/id"
      id
      sendRecord
      silentFallback
  register
    sentryService
    "myLogger"
    Error
    (formatMessage mRequest exception)
    (recordUpdate mRequest exception)
  defaultOnException mRequest exception

formatMessage :: Maybe Request -> SomeException -> String
formatMessage Nothing exception = "Exception before request could be parsed: " ++ show exception
formatMessage (Just request) exception = "Exception " ++ show exception ++ " while handling request " ++ show request

recordUpdate :: Maybe Request -> SomeException -> SentryRecord -> SentryRecord
recordUpdate Nothing _ record = record
recordUpdate (Just request) _ record =
  record
    { srCulprit = Just $ unpack $ rawPathInfo request,
      srServerName = unpack <$> requestHeaderHost request
    }

runSentry :: IO ()
runSentry =
  let settings =
        setPort 8080 $
          setOnException
            sentryOnException
            defaultSettings
   in runSettings settings $ serve (Proxy :: Proxy API) server
