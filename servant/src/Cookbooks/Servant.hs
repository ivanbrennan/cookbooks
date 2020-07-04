{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Cookbooks.Servant
  ( runServer,
  )
where

import Data.Aeson (ToJSON)
import Data.Proxy (Proxy (Proxy))
import Data.Time (Day)
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
    Server,
    serve,
  )

data User
  = User
      { name :: String,
        age :: Int,
        email :: String,
        registration_date :: Day
      }
  deriving (Generic)

instance ToJSON User

type API1 =
  "users"
    :> ( Get '[JSON] [User]
           :<|> Capture "userid" Int :> Get '[JSON] User
       )

server9 :: Server API1
server9 = getUsers :<|> getUser
  where
    getUsers :: Handler [User]
    getUsers = error "..."
    getUser :: Int -> Handler User
    getUser _userid = error "..."

staticAPI :: Proxy API1
staticAPI = Proxy

app9 :: Application
app9 = serve staticAPI server9

runServer :: IO ()
runServer = do
  putStrLn "Starting server"
  run 8081 app9
