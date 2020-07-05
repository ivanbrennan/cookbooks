{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Cookbooks.Servant.Javascript
  ( runJavascriptServer,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (ToJSON)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile, writeFile)
import GHC.Generics (Generic)
import qualified Language.Javascript.JQuery
import Network.Wai.Handler.Warp (run)
import Servant
  ( (:<|>) ((:<|>)),
    (:>),
    Application,
    Get,
    JSON,
    QueryParam,
    Raw,
    Server,
    serve,
    serveDirectoryFileServer,
  )
import Servant.JS (jquery, jsForAPI)
import System.Random (getStdRandom, randomR)

data Point
  = Point
      { x :: Double,
        y :: Double
      }
  deriving (Generic)

instance ToJSON Point

data Search a
  = Search
      { query :: Text,
        results :: [a]
      }
  deriving (Generic)

mkSearch :: Text -> [a] -> Search a
mkSearch = Search

instance ToJSON a => ToJSON (Search a)

data Book
  = Book
      { author :: Text,
        title :: Text,
        year :: Int
      }
  deriving (Generic)

instance ToJSON Book

book :: Text -> Text -> Int -> Book
book = Book

books :: [Book]
books =
  [ book "Paul Hudak" "The Haskell School of Expression: Learning Functional Programming through Multimedia" 2000,
    book "Bryan O'Sullivan, Don Stewart, and John Goerzen" "Real World Haskell" 2008,
    book "Miran Lipovača" "Learn You a Haskell for Great Good!" 2011,
    book "Graham Hutton" "Programming in Haskell" 2007,
    book "Simon Marlow" "Parallel and Concurrent Programming in Haskell" 2013,
    book "Richard Bird" "Introduction to Functional Programming using Haskell" 1998
  ]

searchBook :: Monad m => Maybe Text -> m (Search Book)
searchBook Nothing = pure (mkSearch "" books)
searchBook (Just q) = pure (mkSearch q books')
  where
    books' =
      filter
        ( \b ->
            q' `T.isInfixOf` T.toLower (author b)
              || q' `T.isInfixOf` T.toLower (title b)
        )
        books
    q' = T.toLower q

randomPoint :: MonadIO m => m Point
randomPoint = liftIO . getStdRandom $ \g ->
  let (rx, g') = randomR (-1, 1) g
      (ry, g'') = randomR (-1, 1) g'
   in (Point rx ry, g'')

type API =
  "point" :> Get '[JSON] Point
    :<|> "books" :> QueryParam "q" Text :> Get '[JSON] (Search Book)

type API' = API :<|> "static" :> Raw

api' :: Proxy API'
api' = Proxy

api :: Proxy API
api = Proxy

server :: Server API
server =
  randomPoint
    :<|> searchBook

server' :: Server API'
server' =
  server
    :<|> serveDirectoryFileServer "static"

app :: Application
app = serve api' server'

runJavascriptServer :: IO ()
runJavascriptServer = do
  writeJSFiles
  run 8000 app

apiJS1 :: Text
apiJS1 = jsForAPI api jquery

writeJSFiles :: IO ()
writeJSFiles = do
  T.writeFile "static/api.js" apiJS1
  jq <- T.readFile =<< Language.Javascript.JQuery.file
  T.writeFile "static/jq.js" jq
