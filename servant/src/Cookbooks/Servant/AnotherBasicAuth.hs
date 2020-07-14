{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Cookbooks.Servant.AnotherBasicAuth
  ( anotherBasicAuthMain,
  )
where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (run)
import Servant
  ( (:>),
    BasicAuth,
    BasicAuthCheck (BasicAuthCheck),
    BasicAuthData (BasicAuthData),
    BasicAuthResult (Authorized, BadPassword, NoSuchUser),
    Context ((:.), EmptyContext),
    Get,
    JSON,
    Server,
    basicAuthPassword,
    basicAuthUsername,
    serveWithContext,
  )
import Servant.Client (BaseUrl (BaseUrl), ClientM, Scheme (Http), client, mkClientEnv, runClientM)

type Username = Text

type Password = Text

type Website = Text

data User
  = User
      { user :: Username,
        pass :: Password,
        site :: Website
      }

type UserDB = Map Username User

createUserDB :: [User] -> UserDB
createUserDB users = Map.fromList [(user u, u) | u <- users]

userDB :: UserDB
userDB =
  createUserDB
    [ User "John" "shhhh" "john.com",
      User "foo" "bar" "foobar.net"
    ]

type API = BasicAuth "People's websites" User :> "mysite" :> Get '[JSON] Website

api :: Proxy API
api = Proxy

server :: Server API
server usr = pure (site usr)

checkBasicAuth :: UserDB -> BasicAuthCheck User
checkBasicAuth db = BasicAuthCheck $ \basicAuthData ->
  let username = decodeUtf8 (basicAuthUsername basicAuthData)
      password = decodeUtf8 (basicAuthPassword basicAuthData)
   in case Map.lookup username db of
        Nothing -> pure NoSuchUser
        Just u ->
          if pass u == password
            then pure (Authorized u)
            else pure BadPassword

runApp :: UserDB -> IO ()
runApp db = run 8080 (serveWithContext api ctx server)
  where
    ctx = checkBasicAuth db :. EmptyContext

getSite :: BasicAuthData -> ClientM Website
getSite = client api

anotherBasicAuthMain :: IO ()
anotherBasicAuthMain = do
  mgr <- newManager defaultManagerSettings
  bracket (forkIO $ runApp userDB) killThread $ \_ ->
    runClientM (getSite u) (mkClientEnv mgr (BaseUrl Http "localhost" 8080 ""))
      >>= print
  where
    u = BasicAuthData "foo" "bar"
