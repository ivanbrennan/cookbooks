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
import Servant ((:<|>) ((:<|>)), (:>), Application, Get, JSON, Server, serve)

data User
  = User
      { name :: String,
        age :: Int,
        email :: String,
        registration_date :: Day
      }
  deriving (Generic)

instance ToJSON User

type UserAPI2 =
  "users" :> Get '[JSON] [User]
    :<|> "albert" :> Get '[JSON] User
    :<|> "isaac" :> Get '[JSON] User

isaac :: User
isaac = User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)

albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)

users2 :: [User]
users2 = [isaac, albert]

server2 :: Server UserAPI2
server2 =
  pure users2
    :<|> pure albert
    :<|> pure isaac

userAPI :: Proxy UserAPI2
userAPI = Proxy

app2 :: Application
app2 = serve userAPI server2

runServer :: IO ()
runServer = do
  putStrLn "Starting server"
  run 8081 app2
