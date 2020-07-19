{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module Cookbooks.Servant.STM
  ( runSTMServer,
  )
where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (TVar, atomically, newTVar, readTVar, readTVarIO, writeTVar)
import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (run)
import Servant
  ( (:<|>) ((:<|>)),
    (:>),
    Application,
    Get,
    Handler,
    JSON,
    PostCreated,
    ReqBody,
    ServerT,
    hoistServer,
    serve,
  )
import Servant.Client (BaseUrl (BaseUrl), Scheme (Http), client, mkClientEnv, runClientM)

newtype Book = Book String deriving (Show, Generic)

instance ToJSON Book

instance FromJSON Book

type GetBooks = Get '[JSON] [Book]

type AddBook = ReqBody '[JSON] Book :> PostCreated '[JSON] Book

type BooksAPI = "books" :> (GetBooks :<|> AddBook)

api :: Proxy BooksAPI
api = Proxy

newtype State
  = State
      { books :: TVar [Book]
      }

type AppM = ReaderT State Handler

server :: ServerT BooksAPI AppM
server = getBooks :<|> addBook
  where
    getBooks :: AppM [Book]
    getBooks = do
      State {books} <- ask
      liftIO $ readTVarIO books
    addBook :: Book -> AppM Book
    addBook book = do
      State {books} <- ask
      liftIO $ atomically $ readTVar books >>= writeTVar books . (book :)
      pure book

nt :: State -> AppM a -> Handler a
nt s x = runReaderT x s

app :: State -> Application
app s = serve api $ hoistServer api (nt s) server

runSTMServer :: IO ()
runSTMServer = do
  let port = 8080
  mgr <- newManager defaultManagerSettings
  initialBooks <- atomically $ newTVar []
  let runApp = run port $ app $ State initialBooks
  bracket (forkIO runApp) killThread $ \_ -> do
    let getBooksClient :<|> addBookClient = client api
    let printBooks = getBooksClient >>= liftIO . print
    void $ flip runClientM (mkClientEnv mgr (BaseUrl Http "localhost" port "")) $ do
      void printBooks
      void $ addBookClient $ Book "Harry Potter and the Order of the Phoeni"
      void printBooks
      void $ addBookClient $ Book "To Kill a Mockingbird"
      void printBooks
      void $ addBookClient $ Book "The Picture of Dorian Gray"
      printBooks
