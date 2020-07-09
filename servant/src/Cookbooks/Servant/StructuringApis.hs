{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Cookbooks.Servant.StructuringApis
  ( runServer,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol)
import Network.Wai.Handler.Warp (run)
import Servant
  ( (:<|>) ((:<|>)),
    (:>),
    Capture,
    Get,
    Handler,
    JSON,
    NoContent (NoContent),
    Post,
    QueryParam,
    ReqBody,
    Server,
    serve,
  )

type API =
  FactoringAPI
    :<|> SimpleAPI "users" User UserId
    :<|> SimpleAPI "products" Product ProductId

-- Two endpoints:
--   - GET /x/<some 'Int'>[?y=<some 'Int'>]
--   - POST /x/<some 'Int'>
type FactoringAPI =
  "x" :> Capture "x" Int
    :> ( QueryParam "y" Int :> Get '[JSON] Int
           :<|> Post '[JSON] Int
       )

factoringServer :: Server FactoringAPI
factoringServer x = getXY :<|> postX
  where
    getXY Nothing = pure x
    getXY (Just y) = pure (x + y)
    postX = pure (x - 1)

-- Three endpoints:
--   - GET /<name>
--   - GET /<name>/<some 'i'>
--   - POST /<name>
type SimpleAPI (name :: Symbol) a i =
  name
    :> ( Get '[JSON] [a]
           :<|> Capture "id" i :> Get '[JSON] a
           :<|> ReqBody '[JSON] a :> Post '[JSON] NoContent
       )

simpleServer ::
  Handler [a] ->
  (i -> Handler a) ->
  (a -> Handler NoContent) ->
  Server (SimpleAPI name a i)
simpleServer listAs getA postA =
  listAs :<|> getA :<|> postA

userServer :: Server (SimpleAPI "users" User UserId)
userServer =
  simpleServer
    (pure [])
    ( \userId ->
        pure $
          if userId == 0
            then User "john" 64
            else User "everybody else" 10
    )
    (\_user -> pure NoContent)

productServer :: Server (SimpleAPI "products" Product ProductId)
productServer =
  simpleServer
    (pure [])
    (\_productId -> pure $ Product "Great stuff")
    (\_product -> pure NoContent)

type UserId = Int

data User = User {username :: String, age :: Int}
  deriving (Generic)

instance FromJSON User

instance ToJSON User

type ProductId = Int

newtype Product = Product {productname :: String}
  deriving (Generic)

instance FromJSON Product

instance ToJSON Product

api :: Proxy API
api = Proxy

runServer :: IO ()
runServer =
  run 8080 . serve api $
    factoringServer :<|> userServer :<|> productServer
