{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Cookbooks.Servant.SQLite
  ( runSQLiteApp,
  )
where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy (Proxy))
import Database.SQLite.Simple (Only (Only), execute, execute_, fromOnly, query_, withConnection)
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

type Message = String

type API =
  ReqBody '[PlainText] Message :> Post '[JSON] NoContent
    :<|> Get '[JSON] [Message]

api :: Proxy API
api = Proxy

initDB :: FilePath -> IO ()
initDB dbfile = withConnection dbfile $ \conn ->
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS messages (msg text not null)"

server :: FilePath -> Server API
server dbfile = postMessage :<|> getMessages
  where
    postMessage :: Message -> Handler NoContent
    postMessage msg = do
      liftIO . withConnection dbfile $ \conn ->
        execute
          conn
          "INSERT INTO messages VALUES (?)"
          (Only msg)
      pure NoContent
    getMessages :: Handler [Message]
    getMessages = fmap (map fromOnly) . liftIO
      $ withConnection dbfile
      $ \conn ->
        query_
          conn
          "SELECT msg FROM messages"

postMsg :: Message -> ClientM NoContent

getMsgs :: ClientM [Message]
postMsg :<|> getMsgs = client api

runSQLiteServer :: FilePath -> IO ()
runSQLiteServer dbfile = run 8080 (serve api $ server dbfile)

runSQLiteApp :: IO ()
runSQLiteApp = do
  let dbfile = "test.db"
  initDB dbfile
  mgr <- newManager defaultManagerSettings
  bracket (forkIO $ runSQLiteServer dbfile) killThread $ \_ -> do
    ms <- flip runClientM (mkClientEnv mgr (BaseUrl Http "localhost" 8080 "")) $ do
      void (postMsg "hello")
      void (postMsg "world")
      getMsgs
    print ms
