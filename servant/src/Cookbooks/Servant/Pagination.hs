{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cookbooks.Servant.Pagination
  ( runPaginatedServer,
  )
where

import Data.Aeson (ToJSON, genericToJSON)
import qualified Data.Aeson as Aeson
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import qualified Network.Wai.Handler.Warp as Warp
import Servant
  ( (:>),
    GetPartialContent,
    Handler,
    Header,
    Headers,
    JSON,
    Server,
    addHeader,
    serve,
  )
import Servant.Pagination
  ( HasPagination,
    PageHeaders,
    Range,
    RangeType,
    Ranges,
    applyRange,
    extractRange,
    getDefaultRange,
    getFieldValue,
    returnRange,
  )

data Color
  = Color
      { name :: String,
        rgb :: [Int],
        hex :: String
      }
  deriving (Generic)

instance ToJSON Color where
  toJSON =
    genericToJSON Aeson.defaultOptions

colors :: [Color]
colors =
  [ Color "Black" [0, 0, 0] "#000000",
    Color "Blue" [0, 0, 255] "#0000ff",
    Color "Green" [0, 128, 0] "#008000",
    Color "Grey" [128, 128, 128] "#808080",
    Color "Purple" [128, 0, 128] "#800080",
    Color "Red" [255, 0, 0] "#ff0000",
    Color "Yellow" [255, 255, 0] "#ffff00"
  ]

instance HasPagination Color "name" where
  type RangeType Color "name" = String
  getFieldValue _ = name

-- getRangeOptions :: Proxy "name" -> Proxy Color -> RangeOptions
-- getDefaultRange :: Proxy Color -> Range "name" String

defaultRange :: Range "name" String
defaultRange =
  getDefaultRange (Proxy @Color)

type API =
  "colors"
    :> Header "Range" (Ranges '["name"] Color)
    :> GetPartialContent '[JSON] (Headers MyHeaders [Color])

type MyHeaders =
  Header "Total-Count" Int ': PageHeaders '["name"] Color

server :: Server API
server = handler
  where
    handler :: Maybe (Ranges '["name"] Color) -> Handler (Headers MyHeaders [Color])
    handler mrange = do
      let range =
            fromMaybe defaultRange (mrange >>= extractRange)
      addHeader (length colors) <$> returnRange range (applyRange range colors)

runPaginatedServer :: IO ()
runPaginatedServer =
  Warp.run 1442 $ serve (Proxy @API) server
