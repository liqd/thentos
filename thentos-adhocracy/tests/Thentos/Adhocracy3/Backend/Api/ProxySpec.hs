{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Thentos.Adhocracy3.Backend.Api.ProxySpec where

import Control.Applicative ((<$>))
import Control.Concurrent.Async (Async, async, cancel, wait, link)
import Control.Concurrent.MVar (MVar, newEmptyMVar, readMVar, putMVar)
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
import Network.HTTP.Types (Header, mkStatus)
import Network.Wai (Application, requestBody, rawPathInfo, requestHeaders, requestMethod, responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, setHost, setPort, runSettings)
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec (Spec, describe, context, shouldBe, it, afterAll_, beforeAll)
import Test.QuickCheck (Arbitrary(..), property, Gen, NonEmptyList(..), (==>))

import qualified Data.Text as Text
import qualified Network.Wreq as Wreq

import Thentos.Adhocracy3.Backend.Api.Simple (serveApi)
import Thentos.Test.Core (setupTestBackend)
import Thentos.Test.Types (btsWai)

spec :: Spec
spec = do
    beforeAll setup (afterAll_ teardown tests)

  where
    setup :: IO ()
    setup = do
        let settings = setHost "127.0.0.1" . setPort 8001 $ defaultSettings
        dest <- async $ runSettings settings proxyDestServer
        link dest
        bts <- setupTestBackend serveApi
        let application = bts ^. btsWai
        let settings2 = setHost "127.0.0.1" . setPort 7118 $ defaultSettings
        proxy <- async $ runSettings settings2 application
        putMVar threadsMVar (dest, proxy)

    teardown :: IO ()
    teardown = do
        (dest, proxy) <- readMVar threadsMVar
        cancel proxy
        catch (wait proxy) (\ThreadKilled -> return ())
        cancel dest
        catch (wait dest) (\ThreadKilled -> return ())

    tests :: Spec
    tests = describe "Thentos.Backend.Api.Proxy" $ do
        describe "serviceProxy" $ do

            context "an authenticated request" $ do
                it "is proxied" $ do
                    property $ \rPath -> hitsProxy rPath ==> do
                        let url = "http://localhost:7118/" ++ urlEncode rPath
                        resp <- Wreq.get url
                        resp ^. Wreq.responseStatus . Wreq.statusCode `shouldBe` 299

                it "gets an unchanged body" $ do
                    property $ \rPath (NonEmpty rBody :: NonEmptyList Char) -> hitsProxy rPath ==> do
                        let url = "http://localhost:7118/" ++ urlEncode rPath
                        req <- Wreq.post url (cs rBody :: ByteString)
                        req ^? Wreq.responseBody . key "body"
                            `shouldBe` Just (String $ cs rBody)

                it "gets the proxy path added" $ do
                    property $ \rPath -> hitsProxy rPath ==> do
                        let url = "http://localhost:7118/" ++ urlEncode rPath
                        req <- Wreq.get url
                        req ^? Wreq.responseBody . key "path"
                            `shouldBe` Just (String $ cs $ "/path/" ++ urlEncode rPath)

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

threadsMVar :: MVar (Async (), Async ())
threadsMVar = unsafePerformIO $ newEmptyMVar
{-# NOINLINE threadsMVar #-}

-- * Arbitrary instances

instance Arbitrary Text.Text where
    arbitrary = cs <$> (arbitrary :: Gen String)
