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
    userOperations ::
      Int ->
      ( Handler User
          :<|> (User -> Handler NoContent)
          :<|> Handler NoContent
      )
    userOperations userid =
      viewUser userid :<|> updateUser userid :<|> deleteUser userid
      where
        viewUser :: Int -> Handler User
        viewUser = error "..."
        updateUser :: Int -> User -> Handler NoContent
        updateUser _userid _user = pure NoContent
        deleteUser :: Int -> Handler NoContent
        deleteUser _userid = pure NoContent

newtype Product = Product {productId :: Int} deriving (Generic)

instance ToJSON Product

instance FromJSON Product

type ProductsAPI =
  Get '[JSON] [Product]
    :<|> ReqBody '[JSON] Product :> PostNoContent
    :<|> Capture "productid" Int
      :> ( Get '[JSON] Product
             :<|> ReqBody '[JSON] Product :> PutNoContent
             :<|> DeleteNoContent
         )

productsServer :: Server ProductsAPI
productsServer = getProducts :<|> newProduct :<|> productOperations
  where
    getProducts :: Handler [Product]
    getProducts = pure []
    newProduct :: Product -> Handler NoContent
    newProduct _product = pure NoContent
    productOperations ::
      Int ->
      ( Handler Product
          :<|> (Product -> Handler NoContent)
          :<|> Handler NoContent
      )
    productOperations productid =
      viewProduct productid :<|> updateProduct productid :<|> deleteProduct productid
      where
        viewProduct :: Int -> Handler Product
        viewProduct _productid = error "..."
        updateProduct :: Int -> Product -> Handler NoContent
        updateProduct _productid _product = pure NoContent
        deleteProduct :: Int -> Handler NoContent
        deleteProduct _productid = pure NoContent

type CombinedAPI =
  "users" :> UsersAPI
    :<|> "products" :> ProductsAPI

server10 :: Server CombinedAPI
server10 = usersServer :<|> productsServer

combinedAPI :: Proxy CombinedAPI
combinedAPI = Proxy

app10 :: Application
app10 = serve combinedAPI server10

runServer :: IO ()
runServer = do
  putStrLn "Starting server"
  run 8081 app10
