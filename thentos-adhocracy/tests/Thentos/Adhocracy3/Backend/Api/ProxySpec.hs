{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Thentos.Adhocracy3.Backend.Api.ProxySpec where

import Control.Applicative ((<$>))
import Control.Concurrent.Async (Async, async, cancel, wait, link)
import Control.Exception (catch, AsyncException(ThreadKilled))
import Control.Lens ((^.), (^?))
import Control.Monad (mzero)
import Data.Aeson (ToJSON(..), FromJSON(..), Value(String), encode)
import Data.Aeson.Lens (key)
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (mk, CI(..), original)
import Data.List (isPrefixOf)
import Data.String.Conversions (cs)
import GHC.Generics (Generic)
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Types (Header, mkStatus)
import Network.Socket (PortNumber)
import Network.Wai (Application, requestBody, rawPathInfo, requestHeaders, requestMethod, responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, setHost, setPort, runSettings, runSettingsSocket)
import Test.Hspec (Spec, SpecWith, describe, context, shouldBe, it, afterAll, beforeAll)
import Test.QuickCheck (Arbitrary(..), property, Gen, NonEmptyList(..), (==>))

import qualified Data.Text as Text
import qualified Network.Wreq as Wreq

import Thentos.Adhocracy3.Backend.Api.Simple (serveApi)
import Thentos.Test.Core
import Thentos.Test.Config
import Thentos.Test.Network (openTestSocket)


type Env = (PortNumber, Async (), Async ())

spec :: Spec
spec = do
    beforeAll setup (afterAll teardown tests)

  where
    setup :: IO Env
    setup = do
        let settings = setHost "127.0.0.1" . setPort 8001 $ defaultSettings
        dest <- startDaemon $ runSettings settings proxyDestServer
        mgr <- newManager defaultManagerSettings
        db <- createActionState thentosTestConfig
        let application = serveApi mgr db
        (proxyPort, proxySocket) <- openTestSocket
        proxy <- startDaemon $ runSettingsSocket defaultSettings proxySocket application
        return (proxyPort, dest, proxy)

    teardown :: Env -> IO ()
    teardown (_, dest, proxy) = do
        stopDaemon proxy
        stopDaemon dest

    tests :: SpecWith Env
    tests = describe "Thentos.Backend.Api.Proxy" $ do
        describe "serviceProxy" $ do
            context "an authenticated request" $ do
                it "is proxied" $ \env -> do
                    property $ \rPath -> hitsProxy rPath ==> do
                        resp <- Wreq.get $ url env rPath
                        resp ^. Wreq.responseStatus . Wreq.statusCode `shouldBe` 299

                it "gets an unchanged body" $ \env -> do
                    property $ \rPath (NonEmpty rBody :: NonEmptyList Char) -> hitsProxy rPath ==> do
                        req <- Wreq.post (url env rPath) (cs rBody :: ByteString)
                        req ^? Wreq.responseBody . key "body"
                            `shouldBe` Just (String $ cs rBody)

                it "gets the proxy path added" $ \env -> do
                    property $ \rPath -> hitsProxy rPath ==> do
                        req <- Wreq.get (url env rPath)
                        req ^? Wreq.responseBody . key "path"
                            `shouldBe` Just (String $ cs $ "/path/" ++ urlEncode rPath)
      where
        url (port, _, _) rPath = "http://localhost:" ++ show port ++ "/" ++ urlEncode rPath


-- Check that a path should hit the proxy
hitsProxy :: String -> Bool
hitsProxy = not . or . sequence [ ("principals/users" `isPrefixOf`)
                                , ("activate_account" `isPrefixOf`)
                                , ("login_username" `isPrefixOf`)
                                , ("login_email" `isPrefixOf`)
                                ]


-- * Backend behind proxy

-- Simple server that echoes back all the request it received as a JSON object.
proxyDestServer :: Application
proxyDestServer request respond = do
    bd <- requestBody request
    let reqInfo = RequestInfo { path = rawPathInfo request
                              , headers = requestHeaders request
                              , method = show $ requestMethod request
                              , body = bd
                              }
    -- We pick an unlike status code to easily verify it was sent by the
    -- destination server.
    respond $ responseLBS (mkStatus 299 "") [] (encode reqInfo)

data RequestInfo = RequestInfo
    { path :: ByteString
    , headers :: [Header]
    , method :: String
    , body :: ByteString
    } deriving (Eq, Read, Show, Generic)

-- * Aeson instances

instance ToJSON (CI ByteString) where
    toJSON ci = toJSON (original ci)
instance FromJSON (CI ByteString) where
    parseJSON s@(String _) = mk <$> parseJSON s
    parseJSON _            = mzero

instance FromJSON RequestInfo
instance ToJSON RequestInfo

instance ToJSON ByteString where
    toJSON bs = toJSON (cs bs :: String)
instance FromJSON ByteString where
    parseJSON s@(String _) = cs <$> (parseJSON s :: Parser String)
    parseJSON _            = mzero

-- * Arbitrary instances

instance Arbitrary Text.Text where
    arbitrary = cs <$> (arbitrary :: Gen String)

-- * Starting and stopping background processes

startDaemon :: IO () -> IO (Async ())
startDaemon x = do
    a <- async x
    link a
    return a

stopDaemon :: Async () -> IO ()
stopDaemon a = do
    cancel a
    catch (wait a) (\ThreadKilled -> return ())
