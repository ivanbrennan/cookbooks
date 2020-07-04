{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Cookbooks.Servant
  ( runServer,
  )
where

import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON)
import Data.ByteString (ByteString)
import Data.Proxy (Proxy (Proxy))
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.Wai.Handler.Warp (run)
import Servant
  ( (:<|>) ((:<|>)),
    (:>),
    Application,
    FromHttpApiData,
    Get,
    Handler,
    Header,
    JSON,
    NoContent (NoContent),
    PostNoContent,
    ReqBody,
    Server,
    parseUrlPiece,
    serve,
  )

type API3 =
  Header "Authorization" Token
    :> ( Get '[JSON] SecretData
           :<|> ReqBody '[JSON] SecretData :> PostNoContent
       )

newtype Token = Token ByteString

instance FromHttpApiData Token where
  parseUrlPiece = fmap (Token . encodeUtf8) . parseUrlPiece

newtype SecretData = SecretData ByteString

instance ToJSON SecretData where
  toJSON (SecretData x) = toJSON (decodeUtf8 x)

instance FromJSON SecretData where
  parseJSON = fmap (SecretData . encodeUtf8) . parseJSON

server9 :: Server API3
server9 Nothing = error "..."
server9 (Just _) = getSecretData :<|> addSecretData
  where
    getSecretData :: Handler SecretData
    getSecretData = pure (SecretData "bar")
    addSecretData :: SecretData -> Handler NoContent
    addSecretData _secretData = pure NoContent

staticAPI :: Proxy API3
staticAPI = Proxy

app9 :: Application
app9 = serve staticAPI server9

runServer :: IO ()
runServer = do
  putStrLn "Starting server"
  run 8081 app9
