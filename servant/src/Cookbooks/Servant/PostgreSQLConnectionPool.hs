{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Cookbooks.Servant.PostgreSQLConnectionPool
  ( runPostgreSQLConnectionPool,
  )
where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Pool (Pool, createPool, withResource)
import Data.Proxy (Proxy (Proxy))
import Database.PostgreSQL.Simple
  ( Connection,
    Only (Only),
    close,
    connectPostgreSQL,
    execute,
    execute_,
    fromOnly,
    query_,
  )
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (run)
import Servant
  ( (:<|>) ((:<|>)),
    (:>),
    Get,
    Handler,
    JSON,
    NoContent,
    NoContent (NoContent),
    PlainText,
    Post,
    ReqBody,
    Server,
    serve,
  )
import Servant.Client
  ( BaseUrl (BaseUrl),
    ClientM,
    Scheme (Http),
    client,
    mkClientEnv,
    runClientM,
  )
import System.Environment.Blank (getEnvDefault)

type DBConnectionString = ByteString

type Message = String

type API =
  ReqBody '[PlainText] Message :> Post '[JSON] NoContent
    :<|> Get '[JSON] [Message]

api :: Proxy API
api = Proxy

initDB :: DBConnectionString -> IO ()
initDB connstr = bracket (connectPostgreSQL connstr) close $ \conn ->
  void $
    execute_
      conn
      "CREATE TABLE IF NOT EXISTS messages (msg text not null)"

server :: Pool Connection -> Server API
server conns = postMessage :<|> getMessages
  where
    postMessage :: Message -> Handler NoContent
    postMessage msg = do
      void . liftIO . withResource conns $ \conn ->
        execute
          conn
          "INSERT INTO messages VALUES (?)"
          (Only msg)
      pure NoContent
    getMessages :: Handler [Message]
    getMessages = fmap (map fromOnly) . liftIO
      $ withResource conns
      $ \conn ->
        query_ conn "SELECT msg FROM messages"

runApp :: Pool Connection -> IO ()
runApp conns = run 8080 (serve api $ server conns)

initConnectionPool :: DBConnectionString -> IO (Pool Connection)
initConnectionPool connStr =
  createPool
    (connectPostgreSQL connStr)
    close
    2 -- stripes
    60 -- unused connections are kept open for a minute
    10 -- max 10 connections open per stripe

postMsg :: Message -> ClientM NoContent

getMsgs :: ClientM [Message]
postMsg :<|> getMsgs = client api

runPostgreSQLConnectionPool :: IO ()
runPostgreSQLConnectionPool = do
  user <- getEnvDefault "USER" "postgres"
  dbname <- getEnvDefault "PGDATABASE" "postgres"
  host <- getEnvDefault "PGHOST" "localhost"
  port <- getEnvDefault "PGPORT" "5432"
  let connStr =
        B.pack $
          mconcat
            ["postgres://", user, ":@/", dbname, "?host=", host, "&port=", port]
  pool <- initConnectionPool connStr
  initDB connStr
  mgr <- newManager defaultManagerSettings
  bracket (forkIO $ runApp pool) killThread $ \_ -> do
    ms <- flip runClientM (mkClientEnv mgr (BaseUrl Http "localhost" 8080 "")) $ do
      void (postMsg "hello")
      void (postMsg "world")
      getMsgs
    print ms
