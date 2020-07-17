{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Cookbooks.Servant.FileUpload
  ( runFileUploadServer,
  )
where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client (defaultManagerSettings, httpLbs, newManager, parseRequest)
import Network.HTTP.Client.MultipartFormData (formDataBody, partBS, partFileSource)
import Network.Socket (withSocketsDo)
import Network.Wai.Handler.Warp (run)
import Servant ((:>), JSON, Post, Proxy (Proxy), Server, serve)
import Servant.Multipart
  ( Mem,
    MultipartData,
    MultipartForm,
    fdFileName,
    fdPayload,
    files,
    iName,
    iValue,
    inputs,
  )

type API = MultipartForm Mem (MultipartData Mem) :> Post '[JSON] Integer

api :: Proxy API
api = Proxy

upload :: Server API
upload multipartData = do
  liftIO $ do
    putStrLn "Inputs:"
    forM_ (inputs multipartData) $ \input ->
      putStrLn $
        "  " ++ show (iName input)
          ++ " -> "
          ++ show (iValue input)
    forM_ (files multipartData) $ \file -> do
      let content = fdPayload file
      putStrLn $ "Content of " ++ show (fdFileName file)
      LBS.putStr content
  pure 0

startServer :: IO ()
startServer = run 8080 (serve api upload)

runFileUploadServer :: IO ()
runFileUploadServer = withSocketsDo . bracket (forkIO startServer) killThread $ \_threadid -> do
  manager <- newManager defaultManagerSettings
  req <- parseRequest "http://localhost:8080/"
  resp <- flip httpLbs manager =<< formDataBody form req
  print resp
  where
    form =
      [ partBS "title" "World",
        partBS "text" $ encodeUtf8 "Hello",
        partFileSource "file" "./README.md"
      ]
