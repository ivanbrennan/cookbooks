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
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant ((:>), Application, Get, JSON, Server, serve)
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html5 (ToMarkup, table, td, th, toMarkup, tr)

type PersonAPI = "persons" :> Get '[JSON, HTML] [Person]

data Person
  = Person
      { firstName :: String,
        lastName :: String
      }
  deriving (Generic)

instance ToJSON Person

instance ToMarkup Person where
  toMarkup person =
    tr $ do
      td (toMarkup $ firstName person)
      td (toMarkup $ lastName person)

instance ToMarkup [Person] where
  toMarkup persons = table $ do
    tr $ do
      th "first name"
      th "last name"
    foldMap toMarkup persons

people :: [Person]
people =
  [ Person "Isaac" "Newton",
    Person "Albert" "Einstein"
  ]

server4 :: Server PersonAPI
server4 = pure people

personAPI :: Proxy PersonAPI
personAPI = Proxy

app4 :: Application
app4 = serve personAPI server4

runServer :: IO ()
runServer = do
  putStrLn "Starting server"
  run 8081 app4
