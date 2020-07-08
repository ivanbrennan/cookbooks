{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cookbooks.Servant.Docs
  ( runDocs,
  )
where

import Control.Lens ((&), (.~))
import Cookbooks.Servant.Server
  ( ClientInfo (..),
    Email (..),
    ExampleAPI,
    HelloMessage (..),
    Position (..),
    api,
    emailForClient,
    port,
    server,
  )
import Data.ByteString.Lazy (ByteString)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (encodeUtf8)
import Network.HTTP.Types (ok200)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp (run)
import Servant
  ( (:<|>) ((:<|>)),
    Application,
    Capture,
    QueryParam,
    Raw,
    Server,
    Tagged (Tagged),
    serve,
  )
import Servant.Docs
  ( DocCapture (DocCapture),
    DocIntro (DocIntro),
    DocQueryParam (DocQueryParam),
    ParamKind (Normal),
    ShowContentTypes (FirstContentType),
    ToCapture,
    ToParam,
    ToSample,
    defRenderingOptions,
    docsWithIntros,
    markdownWith,
    requestExamples,
    singleSample,
    toCapture,
    toParam,
    toSamples,
  )

instance ToCapture (Capture "x" Int) where
  toCapture _ =
    DocCapture
      "x"
      "(integer) position on the x axis"

instance ToCapture (Capture "y" Int) where
  toCapture _ =
    DocCapture
      "y"
      "(integer) position on the y axis"

instance ToSample Position where
  toSamples _ = singleSample (Position 3 14)

instance ToParam (QueryParam "name" String) where
  toParam _ =
    DocQueryParam
      "name"
      ["Alp", "John Doe", "..."]
      "Name of the person to say hello to."
      Normal

instance ToSample HelloMessage where
  toSamples _ =
    [ ("When a value is provided for 'name'", HelloMessage "Hello, Alp"),
      ("When 'name' is not specified", HelloMessage "Hello, anonymous")
    ]

ci :: ClientInfo
ci = ClientInfo "Alp" "alp@foo.com" 26 ["haskell", "mathematics"]

instance ToSample ClientInfo where
  toSamples _ = singleSample ci

instance ToSample Email where
  toSamples _ = singleSample (emailForClient ci)

docsBS :: ByteString
docsBS =
  encodeUtf8
    . T.pack
    . markdown'
    $ docsWithIntros [intro] api
  where
    intro = DocIntro "Welcome" ["This is our super webservice's API.", "Enjoy!"]
    markdown' = markdownWith (defRenderingOptions & requestExamples .~ FirstContentType)

type DocsAPI = ExampleAPI :<|> Raw

docsAPI :: Proxy DocsAPI
docsAPI = Proxy

server' :: Server DocsAPI
server' = server :<|> Tagged serveDocs
  where
    serveDocs _ respond =
      respond $ responseLBS ok200 [plain] docsBS
    plain = ("Content-Type", "text/plain")

app :: Application
app = serve docsAPI server'

runDocs :: IO ()
runDocs = run port app
