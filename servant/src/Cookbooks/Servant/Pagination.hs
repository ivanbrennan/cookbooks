{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cookbooks.Servant.Pagination
  ( runPaginatedServer,
  )
where

import Control.Applicative ((<|>))
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
    defaultOptions,
    defaultRangeLimit,
    extractRange,
    getDefaultRange,
    getFieldValue,
    getRangeOptions,
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

instance HasPagination Color "hex" where
  type RangeType Color "hex" = String
  getFieldValue _ = hex

instance HasPagination Color "rgb" where
  type RangeType Color "rgb" = Int
  getFieldValue _ = sum . rgb
  getRangeOptions _ _ = defaultOptions {defaultRangeLimit = 5}

defaultRange :: Range "name" String
defaultRange =
  getDefaultRange (Proxy @Color)

type API =
  "colors"
    :> Header "Range" (Ranges '["name", "hex", "rgb"] Color)
    :> GetPartialContent '[JSON]
         ( Headers
             (Header "Total-Count" Int ': PageHeaders '["name", "hex", "rgb"] Color)
             [Color]
         )

server :: Server API
server = fmap (addHeader (length colors)) . handler
  where
    handler ::
      Maybe (Ranges '["name", "hex", "rgb"] Color) ->
      Handler (Headers (PageHeaders '["name", "hex", "rgb"] Color) [Color])
    handler mrange =
      fromMaybe (returnNameRange defaultRange) $
        fmap returnNameRange (mrange >>= extractRange)
          <|> fmap returnHexRange (mrange >>= extractRange)
          <|> fmap returnRGBRange (mrange >>= extractRange)
    returnNameRange (r :: Range "name" String) = returnRange r (applyRange r colors)
    returnHexRange (r :: Range "hex" String) = returnRange r (applyRange r colors)
    returnRGBRange (r :: Range "rgb" Int) = returnRange r (applyRange r colors)

runPaginatedServer :: IO ()
runPaginatedServer =
  Warp.run 1442 $ serve (Proxy @API) server
