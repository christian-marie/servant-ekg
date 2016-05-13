{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

module Servant.EkgSpec (spec) where

import           Control.Concurrent
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.Monoid
import           Data.Proxy
import qualified Data.HashMap.Strict as H
import           Data.Text
import           GHC.Generics
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Client
import           Servant.API.Internal.Test.ComprehensiveAPI (comprehensiveAPI)
import           System.Metrics
import qualified System.Metrics.Counter as Counter
import           Test.Hspec

import           Servant.Ekg


-- * Spec

spec :: Spec
spec = describe "servant-ekg" $ do

  let getEp :<|> _postEp :<|> _deleteEp = client testApi

  it "collects number of request" $ do
    withApp $ \port mvar -> do
      mgr <- newManager defaultManagerSettings
      _result <- runExceptT $ getEp "name" Nothing mgr (BaseUrl Http "localhost" port "")
      m <- readMVar mvar
      case H.lookup "hello.:name.GET" m of
        Nothing -> fail "Expected some value"
        Just v  -> Counter.read (metersC2XX v) `shouldReturn` 1

  it "is comprehensive" $ do
    let _typeLevelTest = monitorEndpoints comprehensiveAPI undefined undefined undefined
    True `shouldBe` True


-- * Example

-- | A greet message data type
newtype Greet = Greet { _msg :: Text }
  deriving (Generic, Show)

instance FromJSON Greet
instance ToJSON Greet

-- API specification
type TestApi =
       -- GET /hello/:name?capital={true, false}  returns a Greet as JSON
       "hello" :> Capture "name" Text :> QueryParam "capital" Bool :> Get '[JSON] Greet

       -- POST /greet with a Greet as JSON in the request body,
       --             returns a Greet as JSON
  :<|> "greet" :> ReqBody '[JSON] Greet :> Post '[JSON] Greet

       -- DELETE /greet/:greetid
  :<|> "greet" :> Capture "greetid" Text :> Delete '[JSON] ()

testApi :: Proxy TestApi
testApi = Proxy

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'EitherT (Int, String) IO' monad.
server :: Server TestApi
server = helloH :<|> postGreetH :<|> deleteGreetH

  where helloH name Nothing = helloH name (Just False)
        helloH name (Just False) = return . Greet $ "Hello, " <> name
        helloH name (Just True) = return . Greet . toUpper $ "Hello, " <> name

        postGreetH = return

        deleteGreetH _ = return ()

-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
test :: Application
test = serve testApi server

withApp :: (Port -> MVar (H.HashMap Text Meters) -> IO a) -> IO a
withApp a = do
  ekg <- newStore
  ms <- newMVar mempty
  withApplication (return $ monitorEndpoints testApi ekg ms test) $ \p -> a p ms
