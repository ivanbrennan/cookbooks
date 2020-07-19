{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cookbooks.Servant.Authentication
  ( basicAuthMain,
    genAuthMain,
  )
where

import Data.Aeson (ToJSON)
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Wai (Request, requestHeaders)
import Network.Wai.Handler.Warp (run)
import Servant
  ( (:<|>) ((:<|>)),
    (:>),
    AuthProtect,
    BasicAuth,
    BasicAuthCheck (BasicAuthCheck),
    BasicAuthData (BasicAuthData),
    BasicAuthResult (Authorized, Unauthorized),
    Context ((:.), EmptyContext),
    Get,
    Handler,
    JSON,
    Server,
    err401,
    err403,
    errBody,
    serveWithContext,
    throwError,
  )
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import Web.Cookie (parseCookies)

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

newtype Account = Account Text

database :: Map ByteString Account
database =
  Map.fromList
    [ ("key1", Account "Anne Briggs"),
      ("key2", Account "Bruce Cockburn"),
      ("key3", Account "Ghédalia Tazartès")
    ]

lookupAccount :: ByteString -> Handler Account
lookupAccount key = case Map.lookup key database of
  Nothing -> throwError (err403 {errBody = "Invalid Cookie"})
  Just usr -> pure usr

authHandler :: AuthHandler Request Account
authHandler = mkAuthHandler handler
  where
    maybeToEither e = maybe (Left e) Right
    throw401 msg = throwError $ err401 {errBody = msg}
    handler req = either throw401 lookupAccount $ do
      cookie <- maybeToEither "Missing cookie header" $ lookup "cookie" $ requestHeaders req
      maybeToEither "Missing token in cookie" $ lookup "servant-auth-cookie" $ parseCookies cookie

type AuthGenAPI =
  "private" :> AuthProtect "cookie-auth" :> PrivateAPI
    :<|> "public" :> PublicAPI

genAuthAPI :: Proxy AuthGenAPI
genAuthAPI = Proxy

type instance AuthServerData (AuthProtect "cookie-auth") = Account

genAuthServerContext :: Context (AuthHandler Request Account ': '[])
genAuthServerContext = authHandler :. EmptyContext

genAuthServer :: Server AuthGenAPI
genAuthServer =
  let privateDataFunc (Account name) =
        pure (PrivateData ("this is a secret: " <> name))
      publicData = pure [PublicData "this is a public piece of data"]
   in privateDataFunc :<|> publicData

genAuthMain :: IO ()
genAuthMain = run 8080 (serveWithContext genAuthAPI genAuthServerContext genAuthServer)
