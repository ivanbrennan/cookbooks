{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Cookbooks.Servant.Authentication
  ( basicAuthMain,
  )
where

import Data.Aeson (ToJSON)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant
  ( (:<|>) ((:<|>)),
    (:>),
    BasicAuth,
    BasicAuthCheck (BasicAuthCheck),
    BasicAuthData (BasicAuthData),
    BasicAuthResult (Authorized, Unauthorized),
    Context ((:.), EmptyContext),
    Get,
    JSON,
    Server,
    serveWithContext,
  )

newtype PrivateData = PrivateData {ssshhh :: Text}
  deriving (Generic)

instance ToJSON PrivateData

newtype PublicData = PublicData {somedata :: Text}
  deriving (Generic)

instance ToJSON PublicData

newtype User = User {userName :: Text}

type PublicAPI = Get '[JSON] [PublicData]

type PrivateAPI = Get '[JSON] PrivateData

type BasicAPI =
  "public" :> PublicAPI
    :<|> "private" :> BasicAuth "foo-realm" User :> PrivateAPI

basicAuthAPI :: Proxy BasicAPI
basicAuthAPI = Proxy

authCheck :: BasicAuthCheck User
authCheck =
  let check (BasicAuthData username password) =
        if username == "servant" && password == "server"
          then pure (Authorized (User "servant"))
          else pure Unauthorized
   in BasicAuthCheck check

basicAuthServerContext :: Context (BasicAuthCheck User ': '[])
basicAuthServerContext = authCheck :. EmptyContext

basicAuthServer :: Server BasicAPI
basicAuthServer =
  let publicAPIHandler = pure [PublicData "foo", PublicData "bar"]
      privateAPIHandler (user :: User) = pure (PrivateData (userName user))
   in publicAPIHandler :<|> privateAPIHandler

basicAuthMain :: IO ()
basicAuthMain =
  run
    8080
    ( serveWithContext
        basicAuthAPI
        basicAuthServerContext
        basicAuthServer
    )
