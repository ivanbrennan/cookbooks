{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Cookbooks.Servant.HoistedServer
  ( runHoistedServer,
    sampleHandler,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Data.Aeson (FromJSON, ToJSON, defaultOptions, encode, genericToEncoding, toEncoding)
import Data.Default (def)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Network.Wai (Middleware)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger
  ( OutputFormat (CustomOutputFormatWithDetails),
    mkRequestLogger,
    outputFormat,
  )
import Network.Wai.Middleware.RequestLogger.JSON (formatAsJSON)
import Prelude.Compat
import Servant
  ( (:<|>) ((:<|>)),
    (:>),
    Application,
    Context ((:.), EmptyContext),
    Get,
    Handler,
    Header,
    Headers,
    JSON,
    Post,
    ReqBody,
    ServerT,
    err401,
    hoistServerWithContext,
    serveWithContext,
    throwError,
  )
import Servant.Auth (JWT)
import Servant.Auth.Server
  ( AuthResult,
    CookieSettings,
    JWTSettings,
    SetCookie,
    cookieIsSecure,
    defaultCookieSettings,
    defaultJWTSettings,
    generateKey,
  )
import qualified Servant.Auth.Server as SAS
import System.Log.FastLogger
  ( LoggerSet,
    ToLogStr,
    defaultBufSize,
    flushLogStr,
    newStdoutLoggerSet,
    pushLogStrLn,
    toLogStr,
  )
import Prelude ()

port :: Int
port = 3001

data SiteConfig
  = SiteConfig
      { environment :: !Text,
        version :: !Text,
        adminUsername :: !Text,
        adminPasswd :: !Text
      }

data AppCtx
  = AppCtx
      { _getConfig :: SiteConfig,
        _getLogger :: LoggerSet
      }

type AppM = ReaderT AppCtx Handler

data LogMessage
  = LogMessage
      { message :: !Text,
        timestamp :: !UTCTime,
        level :: !Text,
        lversion :: !Text,
        lenvironment :: !Text
      }
  deriving (Generic)

instance FromJSON LogMessage

instance ToJSON LogMessage where
  toEncoding = genericToEncoding defaultOptions

instance ToLogStr LogMessage where
  toLogStr = toLogStr . encode

sampleHandler :: AppM LogMessage
sampleHandler = do
  config <- asks _getConfig
  logset <- asks _getLogger
  tstamp <- liftIO getCurrentTime
  let logMsg =
        LogMessage
          { message = "let's do some logging!",
            timestamp = tstamp,
            level = "info",
            lversion = version config,
            lenvironment = environment config
          }
  liftIO $ pushLogStrLn logset $ toLogStr logMsg
  pure logMsg

newtype AdminUser = AdminUser {name :: Text}
  deriving (Generic)

instance ToJSON AdminUser

instance FromJSON AdminUser

instance SAS.ToJWT AdminUser

instance SAS.FromJWT AdminUser

type AdminApi =
  "admin" :> Get '[JSON] LogMessage

type LoginApi =
  "login"
    :> ReqBody '[JSON] LoginForm
    :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] LogMessage)

data LoginForm
  = LoginForm
      { username :: Text,
        password :: Text
      }
  deriving (Generic)

instance ToJSON LoginForm

instance FromJSON LoginForm

type AdminAndLogin auths = (SAS.Auth auths AdminUser :> AdminApi) :<|> LoginApi

adminServer :: SAS.CookieSettings -> SAS.JWTSettings -> ServerT (AdminAndLogin auths) AppM
adminServer cs jwts = adminHandler :<|> loginHandler cs jwts

adminHandler :: AuthResult AdminUser -> AppM LogMessage
adminHandler (SAS.Authenticated adminUser) = do
  config <- asks _getConfig
  logset <- asks _getLogger
  tstamp <- liftIO getCurrentTime
  let logMsg =
        LogMessage
          { message = "Admin User accessing admin: " <> name adminUser,
            timestamp = tstamp,
            level = "info",
            lversion = version config,
            lenvironment = environment config
          }
  liftIO $ pushLogStrLn logset $ toLogStr logMsg
  pure logMsg
adminHandler _ = throwError err401

loginHandler ::
  CookieSettings ->
  JWTSettings ->
  LoginForm ->
  AppM (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] LogMessage)
loginHandler cookieSettings jwtSettings form = do
  config <- asks _getConfig
  logset <- asks _getLogger
  tstamp <- liftIO getCurrentTime
  let logMsg =
        LogMessage
          { message = "Admin User login attempt failed!",
            timestamp = tstamp,
            level = "info",
            lversion = version config,
            lenvironment = environment config
          }
  case validateLogin config form of
    Nothing -> do
      liftIO $ pushLogStrLn logset $ toLogStr logMsg
      throwError err401
    Just usr -> do
      mApplyCookies <- liftIO $ SAS.acceptLogin cookieSettings jwtSettings usr
      case mApplyCookies of
        Nothing -> do
          liftIO $ pushLogStrLn logset $ toLogStr logMsg
          throwError err401
        Just applyCookies -> do
          let successMsg = logMsg {message = "Admin User successfully authenticated!"}
          liftIO $ pushLogStrLn logset $ toLogStr successMsg
          pure $ applyCookies successMsg

validateLogin :: SiteConfig -> LoginForm -> Maybe AdminUser
validateLogin config (LoginForm uname passwd) =
  if (uname == adminUsername config) && (passwd == adminPasswd config)
    then Just $ AdminUser uname
    else Nothing

adminLoginApi :: Proxy (AdminAndLogin '[JWT])
adminLoginApi = Proxy

mkApp :: Context '[SAS.CookieSettings, SAS.JWTSettings] -> CookieSettings -> JWTSettings -> AppCtx -> Application
mkApp cfg cs jwts ctx =
  serveWithContext adminLoginApi cfg $
    hoistServerWithContext
      adminLoginApi
      (Proxy :: Proxy '[SAS.CookieSettings, SAS.JWTSettings])
      (`runReaderT` ctx)
      (adminServer cs jwts)

jsonRequestLogger :: IO Middleware
jsonRequestLogger =
  mkRequestLogger $ def {outputFormat = CustomOutputFormatWithDetails formatAsJSON}

runHoistedServer :: IO ()
runHoistedServer = do
  let config = SiteConfig "dev" "1.0.0" "admin" "secretPassword"
  warpLogger <- jsonRequestLogger
  appLogger <- newStdoutLoggerSet defaultBufSize
  tstamp <- getCurrentTime
  myKey <- generateKey
  let lgmsg =
        LogMessage
          { message = "My app starting up!",
            timestamp = tstamp,
            level = "info",
            lversion = version config,
            lenvironment = environment config
          }
  pushLogStrLn appLogger (toLogStr lgmsg) >> flushLogStr appLogger
  let ctx = AppCtx config appLogger
      warpSettings = Warp.defaultSettings
      portSettings = Warp.setPort port warpSettings
      settings = Warp.setTimeout 55 portSettings
      jwtCfg = defaultJWTSettings myKey
      cookieCfg =
        if environment config == "dev"
          then defaultCookieSettings {cookieIsSecure = SAS.NotSecure}
          else defaultCookieSettings
      cfg = cookieCfg :. jwtCfg :. EmptyContext
  Warp.runSettings settings $ warpLogger $ mkApp cfg cookieCfg jwtCfg ctx
