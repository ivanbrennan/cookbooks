{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Cookbooks.Servant.Client
  ( runClient,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant
  ( (:<|>) ((:<|>)),
    (:>),
    Capture,
    EmptyAPI,
    Get,
    JSON,
    Post,
    QueryParam,
    ReqBody,
  )
import Servant.Client
  ( BaseUrl (BaseUrl),
    ClientM,
    EmptyClient (EmptyClient),
    Scheme (Http),
    client,
    mkClientEnv,
    runClientM,
  )

data Position
  = Position
      { xCoord :: Int,
        yCoord :: Int
      }
  deriving (Show, Generic)

instance FromJSON Position

newtype HelloMessage = HelloMessage {msg :: String}
  deriving (Show, Generic)

instance FromJSON HelloMessage

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

data Email
  = Email
      { from :: String,
        to :: String,
        subject :: String,
        body :: String
      }
  deriving (Show, Generic)

instance FromJSON Email

type API =
  "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
    :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
    :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

api :: Proxy API
api = Proxy

position ::
  Int ->
  Int ->
  ClientM Position

hello ::
  Maybe String ->
  ClientM HelloMessage

marketing ::
  ClientInfo ->
  ClientM Email
position :<|> hello :<|> marketing = client api

type API' = API :<|> EmptyAPI

api' :: Proxy API'
api' = Proxy

position' ::
  Int ->
  Int ->
  ClientM Position

hello' ::
  Maybe String ->
  ClientM HelloMessage

marketing' ::
  ClientInfo ->
  ClientM Email
(position' :<|> hello' :<|> marketing') :<|> EmptyClient = client api'

queries :: ClientM (Position, HelloMessage, Email)
queries = do
  pos <- position 10 10
  message <- hello (Just "servant")
  em <- marketing (ClientInfo "Alp" "alp@foo.com" 26 ["haskell", "mathematics"])
  pure (pos, message, em)

runClient :: IO ()
runClient = do
  manager' <- newManager defaultManagerSettings
  res <- runClientM queries (mkClientEnv manager' (BaseUrl Http "localhost" 8081 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (pos, message, em) -> do
      print pos
      print message
      print em
