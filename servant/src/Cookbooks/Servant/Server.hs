{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Cookbooks.Servant.Server
  ( ClientInfo (..),
    Email (..),
    HelloMessage (..),
    Position (..),
    ExampleAPI,
    api,
    emailForClient,
    port,
    runServer,
    server,
    IntAPI,
    intAPI,
    runIntServer,
    streamAPI,
    runStreamServer,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.List (intercalate)
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant
  ( (:<|>) ((:<|>)),
    (:>),
    Application,
    Capture,
    FormUrlEncoded,
    Get,
    Handler,
    JSON,
    NewlineFraming,
    Post,
    QueryParam,
    ReqBody,
    Server,
    SourceIO,
    StreamGet,
    serve,
  )
import Servant.Types.SourceT (source)
import Web.FormUrlEncoded (FromForm, ToForm)

port :: Int
port = 8081

data Position
  = Position
      { xCoord :: Int,
        yCoord :: Int
      }
  deriving (Show, Generic)

instance FromJSON Position

instance ToJSON Position

newtype HelloMessage = HelloMessage {msg :: String}
  deriving (Show, Generic)

instance FromJSON HelloMessage

instance ToJSON HelloMessage

data ClientInfo
  = ClientInfo
      { clientName :: String,
        clientEmail :: String,
        clientAge :: Int,
        clientInterestedIn :: [String]
      }
  deriving (Generic)

instance FromJSON ClientInfo

instance ToJSON ClientInfo

instance ToForm ClientInfo

instance FromForm ClientInfo

data Email
  = Email
      { from :: String,
        to :: String,
        subject :: String,
        body :: String
      }
  deriving (Show, Generic)

instance FromJSON Email

instance ToJSON Email

type ExampleAPI =
  "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
    :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
    :<|> "marketing" :> ReqBody '[JSON, FormUrlEncoded] ClientInfo :> Post '[JSON] Email

api :: Proxy ExampleAPI
api = Proxy

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'
  where
    from' = "great@company.com"
    to' = clientEmail c
    subject' = "Hey " ++ clientName c ++ ", we miss you!"
    body' =
      "Hi " ++ clientName c ++ ",\n\n"
        ++ "Since you've recently turned "
        ++ show (clientAge c)
        ++ ", have you checked out our latest "
        ++ intercalate ", " (clientInterestedIn c)
        ++ " products? Give us a visit!"

server :: Server ExampleAPI
server = position :<|> hello :<|> marketing
  where
    position :: Int -> Int -> Handler Position
    position x y = pure (Position x y)
    hello :: Maybe String -> Handler HelloMessage
    hello Nothing = pure $ HelloMessage "Hello, anonymous"
    hello (Just s) = pure $ HelloMessage ("Hello, " ++ s)
    marketing :: ClientInfo -> Handler Email
    marketing c = pure (emailForClient c)

app :: Application
app = serve api server

runServer :: IO ()
runServer = run port app

type IntAPI = Get '[JSON] Int :<|> Capture "n" Int :> Post '[JSON] Int

intAPI :: Proxy IntAPI
intAPI = Proxy

intServer :: Server IntAPI
intServer = getInt :<|> postInt
  where
    getInt :: Handler Int
    getInt = pure 12
    postInt :: Int -> Handler Int
    postInt i = pure (i + 1)

intApp :: Application
intApp = serve intAPI intServer

runIntServer :: IO ()
runIntServer = run port intApp

type StreamAPI = "positionStream" :> StreamGet NewlineFraming JSON (SourceIO Position)

streamAPI :: Proxy StreamAPI
streamAPI = Proxy

streamPositions :: SourceIO Position
streamPositions = source [Position 0 0, Position 0 1, Position 1 0]

streamApp :: Application
streamApp = serve streamAPI (pure streamPositions)

runStreamServer :: IO ()
runStreamServer = run port streamApp
