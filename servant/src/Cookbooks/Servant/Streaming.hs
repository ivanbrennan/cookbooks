{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Cookbooks.Servant.Streaming
  ( runBasicStreaming,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import qualified Network.Wai.Handler.Warp as Warp
import Servant ((:<|>) ((:<|>)), (:>), Application, Capture, JSON, NewlineFraming, NoFraming, OctetStream, Server, SourceIO, StreamBody, StreamGet, StreamPost, serve)
import Servant.Client.Streaming (Client, ClientM, client, mkClientEnv, parseBaseUrl, withClientM)
import qualified Servant.Types.SourceT as S
import System.Environment (getArgs, lookupEnv)
import Text.Read (readMaybe)

type FastAPI = "get" :> Capture "num" Int :> StreamGet NewlineFraming JSON (SourceIO Int)

type API =
  FastAPI
    :<|> "slow" :> Capture "num" Int :> StreamGet NewlineFraming JSON (SourceIO Int)
    :<|> "readme" :> StreamGet NoFraming OctetStream (SourceIO ByteString)
    :<|> "proxy"
      :> StreamBody NoFraming OctetStream (SourceIO ByteString)
      :> StreamPost NoFraming OctetStream (SourceIO ByteString)

api :: Proxy API
api = Proxy

server :: Server API
server = fast :<|> slow :<|> readme :<|> proxy
  where
    fast n = liftIO $ do
      putStrLn $ "/fast/" ++ show n
      pure (fastSource n)
    slow n = liftIO $ do
      putStrLn $ "/slow/" ++ show n
      pure (slowSource n)
    readme = liftIO $ do
      putStrLn "/readme"
      pure (S.readFile "README.md")
    proxy c = liftIO $ do
      putStrLn "/proxy"
      pure c
    fastSource = S.fromStepT . mk
      where
        mk m
          | m < 0 = S.Stop
          | otherwise = S.Yield m (mk (m - 1))
    slowSource m = S.mapStepT delay (fastSource m)
      where
        delay S.Stop = S.Stop
        delay (S.Error err) = S.Error err
        delay (S.Skip s) = S.Skip (delay s)
        delay (S.Effect ms) = S.Effect (fmap delay ms)
        delay (S.Yield x s) =
          S.Effect $
            S.Yield x (delay s) <$ threadDelay 1000000

app :: Application
app = serve api server

cli :: Client ClientM FastAPI
cli :<|> _ :<|> _ :<|> _ = client api

runBasicStreaming :: IO ()
runBasicStreaming = do
  args <- getArgs
  case args of
    ("server" : _) -> do
      putStrLn "Starting servant-basic-streaming server at http://localhost:8000"
      port <- fromMaybe 8000 . (>>= readMaybe) <$> lookupEnv "PORT"
      Warp.run port app
    ("client" : ns : _) -> do
      n <- maybe (fail $ "not a number: " ++ ns) pure $ readMaybe ns
      mgr <- newManager defaultManagerSettings
      burl <- parseBaseUrl "http://localhost:8000/"
      withClientM (cli n) (mkClientEnv mgr burl) $ \case
        Left err -> print err
        Right src -> do
          x <- S.unSourceT src (go (0 :: Int))
          print x
          where
            go !acc S.Stop = pure acc
            go !acc (S.Error err) = print err >> pure acc
            go !acc (S.Skip s) = go acc s
            go !acc (S.Effect ms) = ms >>= go acc
            go !acc (S.Yield _ s) = go (acc + 1) s
    _ -> do
      putStrLn "Try:"
      putStrLn "cabal v2-run servant-basic-streaming server"
      putStrLn "cabal v2-run servant-basic-streaming client 10"
      putStrLn "time curl -H 'Accept: application/json' localhost:8000/slow/5"
