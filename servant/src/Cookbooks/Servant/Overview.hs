{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cookbooks.Servant.Overview
  ( startApp,
    PersonId,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Data.Aeson as JSON
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Database.Persist (Entity (Entity), delete, get, insert, selectList)
import Database.Persist.Postgresql (runMigration, withPostgresqlConn)
import Database.Persist.Sql (SqlPersistT, runSqlConn)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant
  ( (:<|>) ((:<|>)),
    (:>),
    Application,
    Capture,
    Delete,
    Get,
    Handler,
    JSON,
    Post,
    ReqBody,
    Server,
    err404,
    errBody,
    serve,
    throwError,
  )
import System.Environment (getArgs)
import System.Environment.Blank (getEnvDefault)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Person json
    Id   Int Primary Unique
    name Text
    age  Text
    deriving Eq Show Generic
|]

type Api =
  "person" :> Get '[JSON] [Person]
    :<|> "person" :> Capture "id" Int :> Get '[JSON] Person
    :<|> "person" :> Capture "id" Int :> Delete '[JSON] ()
    :<|> "person" :> ReqBody '[JSON] Person :> Post '[JSON] Person

apiProxy :: Proxy Api
apiProxy = Proxy

app :: Application
app = serve apiProxy server

-- Run a database operation and lift the result into a Handler.
-- This minimizes usage of IO operations in other functions.
runDB :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> Handler a
runDB a = liftIO $ do
  connStr <- connectionString
  runNoLoggingT
    $ runResourceT
    $ withPostgresqlConn connStr
    $ runSqlConn a

connectionString :: IO ByteString
connectionString = do
  user <- getEnvDefault "USER" "postgres"
  dbname <- getEnvDefault "PGDATABASE" "postgres"
  host <- getEnvDefault "PGHOST" "localhost"
  port <- getEnvDefault "PGPORT" "5432"
  pure $ B.pack $
    mconcat
      ["postgres://", user, ":@/", dbname, "?host=", host, "&port=", port]

doMigration :: IO ()
doMigration = do
  connStr <- connectionString
  runNoLoggingT
    $ runResourceT
    $ withPostgresqlConn connStr
    $ runReaderT
    $ runMigration migrateAll

server :: Server Api
server =
  personGET
    :<|> personGETById
    :<|> personDELETE
    :<|> personPOST
  where
    personGET = selectPersons
    personGETById = selectPersonById
    personDELETE = deletePerson
    personPOST = createPerson

selectPersons :: Handler [Person]
selectPersons = do
  personList <- runDB $ selectList [] []
  pure $ map (\(Entity _ u) -> u) personList

selectPersonById :: Int -> Handler Person
selectPersonById i = do
  sqlResult <- runDB $ get $ PersonKey i
  case sqlResult of
    Just person -> pure person
    Nothing -> throwError err404 {errBody = JSON.encode "Person with ID not found."}

createPerson :: Person -> Handler Person
createPerson person =
  runDB (insert person) >> pure person

deletePerson :: Int -> Handler ()
deletePerson i = runDB $ delete $ PersonKey i

startApp :: IO ()
startApp = do
  args <- getArgs
  let arg1 = if not (null args) then Just (head args) else Nothing
  case arg1 of
    Just "migrate" -> doMigration
    _ -> run 8080 app
