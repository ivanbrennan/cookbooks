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
import Lucid (ToHtml, table_, td_, th_, toHtml, toHtmlRaw, tr_)
import Network.Wai.Handler.Warp (run)
import Servant ((:>), Application, Get, JSON, Server, serve)
import Servant.HTML.Lucid (HTML)

type PersonAPI = "persons" :> Get '[JSON, HTML] [Person]

data Person
  = Person
      { firstName :: String,
        lastName :: String
      }
  deriving (Generic)

instance ToJSON Person

instance ToHtml Person where
  toHtml person =
    tr_ $ do
      td_ (toHtml $ firstName person)
      td_ (toHtml $ lastName person)

  toHtmlRaw = toHtml

instance ToHtml [Person] where
  toHtml persons = table_ $ do
    tr_ $ do
      th_ "first name"
      th_ "last name"
    foldMap toHtml persons

  toHtmlRaw = toHtml

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
