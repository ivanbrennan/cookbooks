{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Servant ((:>), Application, Get, JSON, Server, serve)

data User
  = User
      { name :: String,
        age :: Int,
        email :: String,
        registration_date :: Day
      }
  deriving (Generic)

instance ToJSON User

type UserAPI1 = "users" :> Get '[JSON] [User]

users1 :: [User]
users1 =
  [ User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1),
    User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)
  ]

server1 :: Server UserAPI1
server1 = pure users1

userAPI :: Proxy UserAPI1
userAPI = Proxy

app1 :: Application
app1 = serve userAPI server1

runServer :: IO ()
runServer = do
  putStrLn "Starting server"
  run 8081 app1
