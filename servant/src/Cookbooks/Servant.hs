{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Cookbooks.Servant
  ( runServer,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy (Proxy (Proxy))
import Data.Time (Day)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant
  ( (:<|>) ((:<|>)),
    (:>),
    Application,
    Get,
    Handler,
    JSON,
    NoContent (NoContent),
    PostNoContent,
    ReqBody,
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

instance FromJSON User

instance ToJSON User

type API2 =
  ReqBody '[JSON] User
    :> ( Get '[JSON] User
           :<|> PostNoContent
       )

server9 :: Server API2
server9 user = getUser :<|> registerUser
  where
    getUser :: Handler User
    getUser = pure user
    registerUser :: Handler NoContent
    registerUser = pure NoContent

staticAPI :: Proxy API2
staticAPI = Proxy

app9 :: Application
app9 = serve staticAPI server9

runServer :: IO ()
runServer = do
  putStrLn "Starting server"
  run 8081 app9
