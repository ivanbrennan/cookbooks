{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Cookbooks.Servant.Server
  ( ClientInfo (..),
    Email (..),
    HelloMessage (..),
    Position (..),
    api,
    runServer,
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
    Get,
    Handler,
    JSON,
    Post,
    QueryParam,
    ReqBody,
    Server,
    serve,
  )

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

type API =
  "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
    :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
    :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

api :: Proxy API
api = Proxy

server :: Server API
server = position :<|> hello :<|> marketing
  where
    position :: Int -> Int -> Handler Position
    position x y = pure (Position x y)
    hello :: Maybe String -> Handler HelloMessage
    hello Nothing = pure $ HelloMessage "Hello, anonymous"
    hello (Just s) = pure $ HelloMessage ("Hello, " ++ s)
    marketing :: ClientInfo -> Handler Email
    marketing c =
      pure $
        Email
          { from = "great@company.com",
            to = clientEmail c,
            subject = "Hey " ++ clientName c ++ ", we miss you!",
            body =
              "Hi "
                ++ clientName c
                ++ ",\n\nSince you've recently turned "
                ++ show (clientAge c)
                ++ ", have you checked out our latest "
                ++ intercalate "," (clientInterestedIn c)
                ++ " products?"
          }

app :: Application
app = serve api server

runServer :: IO ()
runServer = do
  putStrLn "Starting server"
  run 8081 app
