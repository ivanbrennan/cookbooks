{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Cookbooks.Servant
  ( runServer,
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

type API =
  "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
    :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
    :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

data Position
  = Position
      { xCoord :: Int,
        yCoord :: Int
      }
  deriving (Generic)

instance ToJSON Position

newtype HelloMessage = HelloMessage {msg :: String}
  deriving (Generic)

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
  deriving (Generic)

instance ToJSON Email

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

server3 :: Server API
server3 =
  position
    :<|> hello
    :<|> marketing
  where
    position :: Int -> Int -> Handler Position
    position x y = pure (Position x y)
    hello :: Maybe String -> Handler HelloMessage
    hello mname = pure . HelloMessage $ case mname of
      Nothing -> "Hello, anonymous"
      Just n -> "Hello, " ++ n
    marketing :: ClientInfo -> Handler Email
    marketing clientInfo = pure (emailForClient clientInfo)

userAPI :: Proxy API
userAPI = Proxy

app3 :: Application
app3 = serve userAPI server3

runServer :: IO ()
runServer = do
  putStrLn "Starting server"
  run 8081 app3
