{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Cookbooks.Servant
  ( runServer,
  )
where

import Data.Aeson (ToJSON)
import Data.Proxy (Proxy (Proxy))
import Data.Time (Day, fromGregorian)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant
  ( Application,
    Get,
    Header,
    Headers,
    JSON,
    Server,
    addHeader,
    serve,
  )

data User
  = User
      { name :: String,
        age :: Int,
        email :: String,
        registration_date :: Day
      }
  deriving (Generic)

instance ToJSON User

albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)

type MyHandler = Get '[JSON] (Headers '[Header "X-An-Int" Int] User)

myHandler :: Server MyHandler
myHandler = pure (addHeader 1797 albert)

headerAPI :: Proxy MyHandler
headerAPI = Proxy

app7 :: Application
app7 = serve headerAPI myHandler

runServer :: IO ()
runServer = do
  putStrLn "Starting server"
  run 8081 app7
