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
    Capture,
    DeleteNoContent,
    Get,
    Handler,
    JSON,
    NoContent (NoContent),
    PostNoContent,
    PutNoContent,
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

type UsersAPI =
  Get '[JSON] [User]
    :<|> ReqBody '[JSON] User :> PostNoContent
    :<|> Capture "userid" Int
      :> ( Get '[JSON] User
             :<|> ReqBody '[JSON] User :> PutNoContent
             :<|> DeleteNoContent
         )

usersServer :: Server UsersAPI
usersServer = getUsers :<|> newUser :<|> userOperations
  where
    getUsers :: Handler [User]
    getUsers = error "..."
    newUser :: User -> Handler NoContent
    newUser _user = pure NoContent
    userOperations userid =
      viewUser userid :<|> updateUser userid :<|> deleteUser userid
      where
        viewUser :: Int -> Handler User
        viewUser = error "..."
        updateUser :: Int -> User -> Handler NoContent
        updateUser _userid _user = pure NoContent
        deleteUser :: Int -> Handler NoContent
        deleteUser _userid = pure NoContent

usersAPI :: Proxy UsersAPI
usersAPI = Proxy

app9 :: Application
app9 = serve usersAPI usersServer

runServer :: IO ()
runServer = do
  putStrLn "Starting server"
  run 8081 app9
