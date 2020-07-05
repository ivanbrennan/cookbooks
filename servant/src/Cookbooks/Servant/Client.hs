{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Cookbooks.Servant.Client
  ( runClient,
    runHoistedClient,
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
  )
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant ((:<|>) ((:<|>)))
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

runClient :: IO ()
runClient = do
  manager' <- newManager defaultManagerSettings
  res <- runClientM queries (mkClientEnv manager' (BaseUrl Http "localhost" 8081 ""))
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
        getClients (mkClientEnv manager' (BaseUrl Http "localhost" 8081 ""))
  i <- getInt
  j <- postInt i
  print (i, j)
