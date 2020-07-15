{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cookbooks.Servant.JWT
  ( runJWTServer,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import Servant
  ( (:<|>) ((:<|>)),
    (:>),
    Application,
    BasicAuthData (BasicAuthData),
    Capture,
    Context ((:.), EmptyContext),
    Get,
    Handler,
    JSON,
    Server,
    err401,
    serveWithContext,
  )
import qualified Servant as S
import Servant.Auth (Auth, JWT)
import qualified Servant.Auth as SA
import Servant.Auth.Server
  ( AuthResult (Authenticated, Indefinite),
    BasicAuthCfg,
    FromBasicAuthData,
    FromJWT,
    ToJWT,
    defaultCookieSettings,
    defaultJWTSettings,
    fromBasicAuthData,
    generateKey,
    throwAll,
  )
import Servant.Client (BaseUrl (BaseUrl), Scheme (Http), client, mkClientEnv, runClientM)
import System.IO (hPutStrLn, stderr)

port :: Int
port = 3001

data AuthenticatedUser
  = AUser
      { auID :: Int,
        auOrgID :: Int
      }
  deriving (Show, Generic)

instance ToJSON AuthenticatedUser

instance FromJSON AuthenticatedUser

instance ToJWT AuthenticatedUser

instance FromJWT AuthenticatedUser

type Login = ByteString

type Password = ByteString

type DB = Map (Login, Password) AuthenticatedUser

type Connection = DB

type Pool a = a

initConnPool :: IO (Pool Connection)
initConnPool =
  pure $
    Map.fromList
      [ (("user", "pass"), AUser 1 1),
        (("user2", "pass2"), AUser 2 1)
      ]

authCheck ::
  Pool Connection ->
  BasicAuthData ->
  IO (AuthResult AuthenticatedUser)
authCheck connPool (BasicAuthData login password) =
  pure
    $ maybe Indefinite Authenticated
    $ Map.lookup (login, password) connPool

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult AuthenticatedUser)

instance FromBasicAuthData AuthenticatedUser where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData

type TestAPI =
  "foo" :> Capture "i" Int :> Get '[JSON] ()
    :<|> "bar" :> Get '[JSON] ()

type TestAPIServer =
  Auth '[JWT, SA.BasicAuth] AuthenticatedUser :> TestAPI

type TestAPIClient = S.BasicAuth "test" AuthenticatedUser :> TestAPI

testClient :: IO ()
testClient = do
  mgr <- newManager defaultManagerSettings
  let foo :<|> _ =
        client
          (Proxy :: Proxy TestAPIClient)
          (BasicAuthData "name" "pass")
  res <-
    runClientM
      (foo 42)
      (mkClientEnv mgr (BaseUrl Http "localhost" port ""))
  hPutStrLn stderr $ case res of
    Left err -> "Error: " ++ show err
    Right r -> "Success: " ++ show r

server :: Server TestAPIServer
server (Authenticated user) = handleFoo :<|> handleBar
  where
    handleFoo :: Int -> Handler ()
    handleFoo n =
      liftIO $ hPutStrLn stderr $
        concat ["foo: ", show user, " / ", show n]
    handleBar :: Handler ()
    handleBar = liftIO testClient
server _ = throwAll err401

mkApp :: Pool Connection -> IO Application
mkApp connPool = do
  myKey <- generateKey
  let jwtCfg = defaultJWTSettings myKey
      authCfg = authCheck connPool
      cfg = jwtCfg :. defaultCookieSettings :. authCfg :. EmptyContext
      api = Proxy :: Proxy TestAPIServer
  pure $ serveWithContext api cfg server

runJWTServer :: IO ()
runJWTServer = do
  connPool <- initConnPool
  let settings =
        setPort port $
          setBeforeMainLoop
            ( hPutStrLn
                stderr
                ("listening on port " ++ show port)
            )
            defaultSettings
  runSettings settings =<< mkApp connPool
