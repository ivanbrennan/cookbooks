{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Cookbooks.Servant.Client
  ( runClient,
    runHoistedClient,
    runStreamClient,
  )
where

import Cookbooks.Servant.Server
  ( ClientInfo (ClientInfo),
    Email,
    HelloMessage,
    IntAPI,
    Position,
    api,
    intAPI,
    streamAPI,
  )
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant ((:<|>) ((:<|>)), SourceIO)
import Servant.Client
  ( BaseUrl (BaseUrl),
    Client,
    ClientEnv,
    ClientM,
    Scheme (Http),
    client,
    hoistClient,
    mkClientEnv,
    runClientM,
  )
import qualified Servant.Client.Streaming as S
import Servant.Types.SourceT (foreach)

position ::
  Int ->
  Int ->
  ClientM Position

hello ::
  Maybe String ->
  ClientM HelloMessage

marketing ::
  ClientInfo ->
  ClientM Email
position :<|> hello :<|> marketing = client api

queries :: ClientM (Position, HelloMessage, Email)
queries = do
  pos <- position 10 10
  message <- hello (Just "servant")
  em <- marketing (ClientInfo "Alp" "alp@foo.com" 26 ["haskell", "mathematics"])
  pure (pos, message, em)

baseUrl :: BaseUrl
baseUrl = BaseUrl Http "localhost" 8081 ""

runClient :: IO ()
runClient = do
  manager' <- newManager defaultManagerSettings
  res <- runClientM queries (mkClientEnv manager' baseUrl)
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (pos, message, em) -> do
      print pos
      print message
      print em

getClients :: ClientEnv -> Client IO IntAPI
getClients clientEnv =
  hoistClient
    intAPI
    ( fmap (either (error . show) id)
        . flip runClientM clientEnv
    )
    (client intAPI)

runHoistedClient :: IO ()
runHoistedClient = do
  manager' <- newManager defaultManagerSettings
  let getInt :: IO Int
      postInt :: Int -> IO Int
      getInt :<|> postInt =
        getClients (mkClientEnv manager' baseUrl)
  i <- getInt
  j <- postInt i
  print (i, j)

posStream :: S.ClientM (SourceIO Position)
posStream = S.client streamAPI

printSourceIO :: Show a => ClientEnv -> S.ClientM (SourceIO a) -> IO ()
printSourceIO env c = S.withClientM c env $ \case
  Left err -> putStrLn $ "Error: " ++ show err
  Right rs -> foreach fail print rs

runStreamClient :: IO ()
runStreamClient = do
  manager' <- newManager defaultManagerSettings
  printSourceIO (mkClientEnv manager' baseUrl) posStream
