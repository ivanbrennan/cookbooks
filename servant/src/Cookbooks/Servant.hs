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

type UserAPI4 =
  Capture "userid" Int
    :> ( Get '[JSON] User
           :<|> DeleteNoContent
       )

server9 :: Server UserAPI4
server9 userid = getUser userid :<|> deleteUser userid
  where
    getUser :: Int -> Handler User
    getUser = error "..."
    deleteUser :: Int -> Handler NoContent
    deleteUser = error "..."

staticAPI :: Proxy UserAPI4
staticAPI = Proxy

app9 :: Application
app9 = serve staticAPI server9

runServer :: IO ()
runServer = do
  putStrLn "Starting server"
  run 8081 app9
