{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Control.Concurrent as C
import Control.Concurrent.MVar (newMVar, readMVar, swapMVar)
import Control.Exception (bracket)
import Control.Lens ((^?), ix)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON, Value (Object, String), decode)
import Data.Aeson.Lens (_Object)
import Data.Either (isRight)
import qualified Data.HashMap.Strict as HM
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (Header, methodPost)
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Prelude.Compat
import Servant
  ( (:<|>) ((:<|>)),
    (:>),
    BasicAuth,
    BasicAuthCheck (BasicAuthCheck),
    BasicAuthResult (NoSuchUser),
    Capture,
    Context ((:.), EmptyContext),
    Get,
    Handler,
    JSON,
    Post,
    ReqBody,
    Server,
    err400,
    err404,
    err500,
    errBody,
    serve,
    throwError,
  )
import Servant.Client
  ( ClientEnv,
    ClientError,
    ClientM,
    baseUrlPort,
    client,
    mkClientEnv,
    parseBaseUrl,
    runClientM,
  )
import Servant.QuickCheck
  ( (<%>),
    Args,
    defaultArgs,
    getsHaveCacheControlHeader,
    maxSuccess,
    not500,
    notAllowedContainsAllowHeader,
    onlyJsonObjects,
    serverSatisfies,
    unauthorizedContainsWWWAuthenticate,
    withServantServerAndContext,
  )
import Servant.QuickCheck.Internal (serverDoesntSatisfy)
import Servant.Server ()
import Test.Hspec (Spec, around, around_, describe, hspec, it, runIO, shouldBe)
import Test.Hspec.Wai (Body, get, request, shouldRespondWith, with)
import Test.Hspec.Wai.Matcher (MatchBody (MatchBody), matchBody)
import Prelude ()

data User
  = User
      { name :: Text,
        user_id :: Integer
      }
  deriving (Show, Eq, Generic)

instance FromJSON User

instance ToJSON User

type UserAPI =
  "user" :> Capture "userId" Integer :> Post '[JSON] User

userApp :: Application
userApp = serve (Proxy :: Proxy UserAPI) userServer

userServer :: Server UserAPI
userServer = createUser

createUser :: Integer -> Handler User
createUser userId =
  if userId > 5000
    then pure $ User {name = "some user", user_id = userId}
    else throwError $ err400 {errBody = "userId is too small"}

withUserApp :: (Warp.Port -> IO ()) -> IO ()
withUserApp =
  Warp.testWithApplication (pure userApp)

businessLogicSpec :: Spec
businessLogicSpec =
  around withUserApp $ do
    let createUser' = client (Proxy :: Proxy UserAPI)
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv' port = mkClientEnv manager (baseUrl {baseUrlPort = port})
    describe "POST /user" $ do
      it "should create a user with a high enough ID" $ \port -> do
        result <- runClientM (createUser' 5001) (clientEnv' port)
        result `shouldBe` (Right $ User {name = "some user", user_id = 5001})
      it "should not create a user with too small an ID" $ \port -> do
        result <- runClientM (createUser' 4999) (clientEnv' port)
        isRight result `shouldBe` False

type SearchAPI =
  "myIndex" :> "myDocType" :> Capture "docId" Integer :> Get '[JSON] Value

getDocument :: Integer -> ClientM Value
getDocument = client (Proxy :: Proxy SearchAPI)

clientEnv :: Text -> Text -> IO ClientEnv
clientEnv esHost esPort = do
  baseUrl <- parseBaseUrl $ unpack $ esHost <> ":" <> esPort
  manager <- newManager defaultManagerSettings
  pure $ mkClientEnv manager baseUrl

runSearchClient :: Text -> Text -> ClientM a -> IO (Either ClientError a)
runSearchClient esHost esPort = (clientEnv esHost esPort >>=) . runClientM

type DocApi =
  "docs" :> Capture "docId" Integer :> Get '[JSON] Value

docsApp :: Text -> Text -> Application
docsApp esHost esPort = serve (Proxy :: Proxy DocApi) $ docServer esHost esPort

docServer :: Text -> Text -> Server DocApi
docServer = getDocsById

getDocsById :: Text -> Text -> Integer -> Handler Value
getDocsById esHost esPort docId = do
  docRes <- liftIO $ runSearchClient esHost esPort (getDocument docId)
  case docRes of
    Left _err -> throwError $ err404 {errBody = "Failed looking up content"}
    Right value ->
      case value ^? _Object . ix "_source" of
        Nothing -> throwError $ err400 {errBody = "Failed parsing content"}
        Just obj -> pure obj

withElasticsearch :: IO () -> IO ()
withElasticsearch action =
  bracket
    (liftIO $ C.forkIO $ Warp.run 9999 esTestApp)
    C.killThread
    (const action)

esTestApp :: Application
esTestApp = serve (Proxy :: Proxy SearchAPI) esTestServer

esTestServer :: Server SearchAPI
esTestServer = getESDocument

getESDocument :: Integer -> Handler Value
getESDocument docId
  | docId > 1000 = throwError err500
  | docId > 500 = pure . Object $ HM.fromList [("bad", String "data")]
  | otherwise = pure $ Object $ HM.fromList [("_source", Object $ HM.fromList [("a", String "b")])]

thirdPartyResourcesSpec :: Spec
thirdPartyResourcesSpec = around_ withElasticsearch
  $ with (pure $ docsApp "localhost" "9999")
  $ describe "GET /docs"
  $ do
    it "should be able to get a document" $
      get "/docs/1" `shouldRespondWith` 200
    it "should be able to handle connection failures" $
      get "/docs/1001" `shouldRespondWith` 404
    it "should be able to handle parsing failures" $
      get "/docs/501" `shouldRespondWith` 400
    it "should be able to handle odd HTTP requests" $
      request methodPost "/docs/501" [] "{" `shouldRespondWith` 405
    it "we can do more with the Response using hspec-wai's matchers" $
      get "/docs/1" `shouldRespondWith` 200 {matchBody = MatchBody bodyMatcher}

bodyMatcher :: [Header] -> Body -> Maybe String
bodyMatcher _ body = case (decode body :: Maybe Value) of
  Just val | val == Object (HM.fromList [("a", String "b")]) -> Nothing
  _ -> Just "This is how we represent failure: this message will be printed"

type API =
  ReqBody '[JSON] String :> Post '[JSON] String
    :<|> Get '[JSON] Int
    :<|> BasicAuth "some-realm" () :> Get '[JSON] ()

api :: Proxy API
api = Proxy

server :: IO (Server API)
server = do
  mvar <- newMVar ""
  pure $
    liftIO . swapMVar mvar
      :<|> liftIO (readMVar mvar >>= pure . length)
      :<|> const (pure ())

args :: Args
args = defaultArgs {maxSuccess = 500}

ctx :: Context '[BasicAuthCheck ()]
ctx = BasicAuthCheck (const . pure $ NoSuchUser) :. EmptyContext

servantQuickcheckSpec :: Spec
servantQuickcheckSpec = describe "" $ do
  it "API demonstrates best practices"
    $ withServantServerAndContext api ctx server
    $ \burl ->
      serverSatisfies
        api
        burl
        args
        ( unauthorizedContainsWWWAuthenticate
            <%> not500
            <%> onlyJsonObjects
            <%> mempty
        )
  it "API doesn't have these things implemented yet"
    $ withServantServerAndContext api ctx server
    $ \burl ->
      serverDoesntSatisfy
        api
        burl
        args
        ( getsHaveCacheControlHeader
            <%> notAllowedContainsAllowHeader
            <%> mempty
        )

spec :: Spec
spec = do
  businessLogicSpec
  thirdPartyResourcesSpec
  servantQuickcheckSpec

main :: IO ()
main = hspec spec
