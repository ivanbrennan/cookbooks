{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Cookbooks.Servant.Server
  ( runServer,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy (Proxy (Proxy))
import Data.Time (Day, fromGregorian)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant
  ( (:>),
    Application,
    JSON,
    NewlineFraming,
    SourceIO,
    StreamGet,
    serve,
  )
import Servant.Types.SourceT (source)

data User
  = User
      { name :: String,
        age :: Int,
        email :: String,
        registration_date :: Day
      }
  deriving (Generic)

instance FromJSON User

instance ToJSON User

isaac :: User
isaac = User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)

albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)

type StreamAPI = "userStream" :> StreamGet NewlineFraming JSON (SourceIO User)

streamAPI :: Proxy StreamAPI
streamAPI = Proxy

streamUsers :: SourceIO User
streamUsers = source [isaac, albert, albert]

app :: Application
app = serve streamAPI (pure streamUsers)

runServer :: IO ()
runServer = do
  putStrLn "Starting server"
  run 8081 app
