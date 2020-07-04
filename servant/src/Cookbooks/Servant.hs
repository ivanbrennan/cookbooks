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
    EmptyAPI,
    Get,
    Handler,
    JSON,
    NoContent (NoContent),
    PostNoContent,
    PutNoContent,
    ReqBody,
    Server,
    emptyServer,
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

newtype Product = Product {productId :: Int} deriving (Generic)

instance ToJSON Product

instance FromJSON Product

type APIFor a i =
  Get '[JSON] [a]
    :<|> ReqBody '[JSON] a :> PostNoContent
    :<|> Capture "id" i
      :> ( Get '[JSON] a
             :<|> ReqBody '[JSON] a :> PutNoContent
             :<|> DeleteNoContent
         )

serverFor ::
  Handler [a] ->
  (a -> Handler NoContent) ->
  (i -> Handler a) ->
  (i -> a -> Handler NoContent) ->
  (i -> Handler NoContent) ->
  Server (APIFor a i)
serverFor list create view update delete =
  list :<|> create :<|> operations
  where
    operations i = view i :<|> update i :<|> delete i

type UsersAPI = APIFor User Int

usersServer :: Server UsersAPI
usersServer =
  serverFor getUsers newUser viewUser updateUser deleteUser
  where
    getUsers :: Handler [User]
    getUsers = error "..."
    newUser :: User -> Handler NoContent
    newUser _user = pure NoContent
    viewUser :: Int -> Handler User
    viewUser = error "..."
    updateUser :: Int -> User -> Handler NoContent
    updateUser _userid _user = pure NoContent
    deleteUser :: Int -> Handler NoContent
    deleteUser _userid = pure NoContent

type ProductsAPI = APIFor Product Int

productsServer :: Server ProductsAPI
productsServer =
  serverFor getProducts newProduct viewProduct updateProduct deleteProduct
  where
    getProducts :: Handler [Product]
    getProducts = pure []
    newProduct :: Product -> Handler NoContent
    newProduct _product = pure NoContent
    viewProduct :: Int -> Handler Product
    viewProduct _productid = error "..."
    updateProduct :: Int -> Product -> Handler NoContent
    updateProduct _productid _product = pure NoContent
    deleteProduct :: Int -> Handler NoContent
    deleteProduct _productid = pure NoContent

type CombinedAPI =
  "users" :> UsersAPI
    :<|> "products" :> ProductsAPI
    :<|> "empty" :> EmptyAPI

server10 :: Server CombinedAPI
server10 = usersServer :<|> productsServer :<|> emptyServer

combinedAPI :: Proxy CombinedAPI
combinedAPI = Proxy

app10 :: Application
app10 = serve combinedAPI server10

runServer :: IO ()
runServer = do
  putStrLn "Starting server"
  run 8081 app10
