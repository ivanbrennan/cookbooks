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
    DeleteNoContent,
    Get,
    Handler,
    JSON,
    NoContent,
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

type UserAPI3 =
  Capture "userid" Int :> Get '[JSON] User
    :<|> Capture "userid" Int :> DeleteNoContent

server8 :: Server UserAPI3
server8 = getUser :<|> deleteUser
  where
    getUser :: Int -> Handler User
    getUser _userid = error "..."
    deleteUser :: Int -> Handler NoContent
    deleteUser _userid = error "..."

staticAPI :: Proxy UserAPI3
staticAPI = Proxy

app8 :: Application
app8 = serve staticAPI server8

runServer :: IO ()
runServer = do
  putStrLn "Starting server"
  run 8081 app8
