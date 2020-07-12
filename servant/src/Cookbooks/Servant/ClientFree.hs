{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Cookbooks.Servant.ClientFree
  ( main,
  )
where

import Control.Monad.Free (Free (Free, Pure))
import Data.Proxy (Proxy (Proxy))
import qualified Network.HTTP.Client as HTTP
import Network.Wai.Handler.Warp (run)
import Servant ((:>), Capture, Get, JSON, serve)
import Servant.Client.Free (ClientF (RunRequest, Throw), client, parseBaseUrl)
import qualified Servant.Client.Internal.HttpClient as I
import System.Environment (getArgs)

type API = "square" :> Capture "n" Int :> Get '[JSON] Int

api :: Proxy API
api = Proxy

getSquare :: Int -> Free ClientF Int
getSquare = client api

test :: IO ()
test = case getSquare 42 of
  Pure n ->
    putStrLn $ "ERROR: got pure result: " ++ show n
  Free (Throw err) ->
    putStrLn $ "ERROR: got error right away: " ++ show err
  Free (RunRequest req k) -> do
    burl <- parseBaseUrl "http://localhost:8000"
    mgr <- HTTP.newManager HTTP.defaultManagerSettings
    let req' = I.defaultMakeClientRequest burl req
    putStrLn $ "Making request: " ++ show req'
    res' <- HTTP.httpLbs req' mgr
    putStrLn $ "Got response: " ++ show res'
    let res = I.clientResponseToResponse id res'
    case k res of
      Pure n ->
        putStrLn $ "Expected 1764, got " ++ show n
      _ ->
        putStrLn "ERROR: didn't get a response"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("server" : _) -> do
      putStrLn "Starting servant-client-free at http://localhost:8000"
      run 8000 $ serve api $ \n -> pure (n * n)
    ("client" : _) ->
      test
    _ -> do
      putStrLn "Try:"
      putStrLn "cabal v2-run servant-client-free server"
      putStrLn "cabal v2-run servant-client-free client"
