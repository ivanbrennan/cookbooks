{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Cookbooks.Servant.Curl
  ( runCurl,
  )
where

import Control.Lens ((^.))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Text (encodeToLazyText)
import Data.Monoid ()
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as T.IO
import qualified Data.Text.Lazy as LazyT
import GHC.Generics (Generic)
import Servant ((:<|>), (:>), Get, JSON, Post, ReqBody)
import Servant.Foreign
  ( Foreign,
    GenerateList,
    HasForeign,
    HasForeignType,
    Req,
    Segment,
    SegmentType (Cap, Static),
    argName,
    listFromAPI,
    path,
    reqBody,
    reqMethod,
    reqUrl,
    typeFor,
    unPathSegment,
    unSegment,
  )
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary, shrink)
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)
import Test.QuickCheck.Gen (generate)

data User
  = User
      { name :: String,
        age :: Int,
        email :: String
      }
  deriving (Generic)

instance Arbitrary User where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance ToJSON User

instance FromJSON User

type UserAPI =
  "users" :> Get '[JSON] [User]
    :<|> "new" :> "user" :> ReqBody '[JSON] User :> Post '[JSON] ()

api :: Proxy UserAPI
api = Proxy

data NoLang

newtype Mocked = Mocked (IO Text)

instance (ToJSON a, Arbitrary a) => HasForeignType NoLang Mocked a where
  typeFor _ _ _ =
    Mocked (genText (Proxy :: Proxy a))

genText :: (ToJSON a, Arbitrary a) => Proxy a -> IO Text
genText =
  fmap (LazyT.toStrict . encodeToLazyText) . genArb

genArb :: Arbitrary a => Proxy a -> IO a
genArb _ =
  generate arbitrary

generateCurl ::
  (GenerateList Mocked (Foreign Mocked api), HasForeign NoLang Mocked api) =>
  Proxy api ->
  Text ->
  IO Text
generateCurl p host =
  fmap T.unlines body
  where
    body =
      foldr (\endp curlCalls -> mCons (generateEndpoint host endp) curlCalls) (pure []) $
        listFromAPI (Proxy :: Proxy NoLang) (Proxy :: Proxy Mocked) p

mCons :: IO a -> IO [a] -> IO [a]
mCons ele list =
  ele >>= \e -> list >>= \l -> pure (e : l)

generateEndpoint :: Text -> Req Mocked -> IO Text
generateEndpoint host req =
  case maybeBody of
    Just body ->
      body >>= \b ->
        pure $
          T.intercalate
            " "
            [ "curl",
              "-X",
              method,
              "-d",
              "'" <> b <> "'",
              "-H 'Content-Type: application/json'",
              host <> "/" <> url
            ]
    Nothing ->
      pure $ T.intercalate " " ["curl", "-X", method, host <> "/" <> url]
  where
    method = decodeUtf8 $ req ^. reqMethod
    url = T.intercalate "/" $ map segment (req ^. reqUrl . path)
    maybeBody = fmap (\(Mocked io) -> io) (req ^. reqBody)

segment :: Segment Mocked -> Text
segment seg =
  case unSegment seg of
    Static p ->
      unPathSegment p
    Cap arg ->
      unPathSegment $ arg ^. argName

runCurl :: IO ()
runCurl =
  generateCurl api "localhost:8081" >>= T.IO.putStrLn
