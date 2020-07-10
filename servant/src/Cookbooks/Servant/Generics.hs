{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Cookbooks.Servant.Generics
  ( runGenericServer,
    cliGet,
    api',
    getLink,
    routesLinks,
    apiMyMonad,
  )
where

import Control.Exception (throwIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (run)
import Servant
  ( (:>),
    Application,
    AsLink,
    Capture,
    Get,
    Handler,
    JSON,
    Link,
    Put,
    ReqBody,
    allFieldLinks,
    fieldLink,
  )
import Servant.API.Generic ((:-), ToServantApi, genericApi)
import Servant.Client (BaseUrl (BaseUrl), Scheme (Http), mkClientEnv, runClientM)
import Servant.Client.Generic (AsClientT, genericClientHoist)
import Servant.Server.Generic (AsServer, AsServerT, genericServe, genericServeT)
import System.Environment (getArgs)

data Routes route
  = Routes
      { _get :: route :- Capture "id" Int :> Get '[JSON] String,
        _put :: route :- ReqBody '[JSON] Int :> Put '[JSON] Bool
      }
  deriving (Generic)

api' :: Proxy (ToServantApi Routes)
api' = genericApi (Proxy :: Proxy Routes)

getLink :: Int -> Link
getLink = fieldLink _get

routesLinks :: Routes (AsLink Link)
routesLinks = allFieldLinks

baseUrl :: BaseUrl
baseUrl = BaseUrl Http "localhost" 8000 ""

cliRoutes :: Manager -> Routes (AsClientT IO)
cliRoutes mgr =
  genericClientHoist $ \x ->
    runClientM x (mkClientEnv mgr baseUrl) >>= either throwIO pure

cliGet :: Int -> IO String
cliGet i = do
  mgr <- newManager defaultManagerSettings
  _get (cliRoutes mgr) i

record :: Routes AsServer
record =
  Routes
    { _get = pure . show,
      _put = pure . odd
    }

app :: Application
app = genericServe record

data AppCustomState
  = AppCustomState

type AppM = ReaderT AppCustomState Handler

apiMyMonad :: Proxy (ToServantApi Routes)
apiMyMonad = genericApi (Proxy :: Proxy Routes)

getRouteMyMonad :: Int -> AppM String
getRouteMyMonad = pure . show

putRouteMyMonad :: Int -> AppM Bool
putRouteMyMonad = pure . odd

recordMyMonad :: Routes (AsServerT AppM)
recordMyMonad = Routes {_get = getRouteMyMonad, _put = putRouteMyMonad}

nt :: AppCustomState -> AppM a -> Handler a
nt s x = runReaderT x s

appMyMonad :: AppCustomState -> Application
appMyMonad state = genericServeT (nt state) recordMyMonad

runGenericServer :: IO ()
runGenericServer = do
  args <- getArgs
  case args of
    ("run" : _) -> do
      putStrLn "Starting generic server at http://localhost:8000"
      run 8000 app
    ("run-custom-monad" : _) -> do
      putStrLn "Starting generic server with a custom monad at http://localhost:8000"
      run 8000 (appMyMonad AppCustomState)
    _ -> putStrLn "To run, pass 'run' argument"
