{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Thentos.Backend.Api.CaptchaSpec (spec) where

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, tryTakeMVar, readMVar)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Configifier (Tagged(Tagged), (>>.))
import Data.Pool (Pool)
import Data.Proxy (Proxy(Proxy))
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Network.Wai (Application)
import Network.Wai.Test (simpleBody, simpleStatus)
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.Status (Status(statusCode))
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec (Spec, SpecWith, describe, it, shouldBe)
import Test.Hspec.Wai (with, request)

import Thentos.Action.Types
import Thentos.Backend.Api.Captcha (serveApi)
import Thentos.Test.Config
import Thentos.Test.Core
import Thentos.Test.Transaction
import Thentos.Types

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS

captchaApp :: IO Application
captchaApp = do
    as@(ActionState cfg _ connPool) <- createActionState "test_thentos" thentosTestConfig
    void $ tryTakeMVar connPoolVar -- discard old value, if any
    putMVar connPoolVar connPool
    let Just beConfig = Tagged <$> cfg >>. (Proxy :: Proxy '["backend"])
    return $! serveApi beConfig as

spec :: Spec
spec = describe "Thentos.Backend.Api.Simple" $ with captchaApp specRest

connPoolVar :: MVar (Pool Connection)
connPoolVar = unsafePerformIO $ newEmptyMVar
{-# NOINLINE connPoolVar #-}

specRest :: SpecWith Application
specRest = do
    describe "/captcha" $ do
        it "returns a png image on empty POST request" $ do
            res <- request "POST" "/captcha" [] ""
            liftIO $ statusCode (simpleStatus res) `shouldBe` 201
            liftIO $ LBS.length (simpleBody res) > 0 `shouldBe` True
            connPool :: Pool Connection <- liftIO $ readMVar connPoolVar
            liftIO $ rowCountShouldBe connPool "captchas" 1

    describe "/audio_captcha" $ do
        it "returns a sound file on empty POST request" $ do
            res <- request "POST" "/audio_captcha/en" [] ""
            liftIO $ statusCode (simpleStatus res) `shouldBe` 201
            liftIO $ LBS.length (simpleBody res) > 0 `shouldBe` True
            connPool :: Pool Connection <- liftIO $ readMVar connPoolVar
            liftIO $ rowCountShouldBe connPool "captchas" 1

    describe "/solve_captcha" $ do
        it "returns false when the captcha id does not exist" $ do
            let captchaSolution = Aeson.encode $ CaptchaSolution (CaptchaId "id") "solution"
            res <- request "POST" "/solve_captcha" jsonHeaders captchaSolution
            let Just (JsonTop (captchaCorrect :: Bool)) = Aeson.decode (simpleBody res)
            liftIO $ statusCode (simpleStatus res) `shouldBe` 201
            liftIO $ captchaCorrect `shouldBe` False

        it "returns True if the correct solution is posted" $ do
            connPool :: Pool Connection <- liftIO $ readMVar connPoolVar
            void . liftIO $ doTransaction connPool
                [sql| INSERT INTO captchas (id, solution)
                      VALUES ('id', 'solution') |] ()
            let captchaSolution = Aeson.encode $ CaptchaSolution (CaptchaId "id") "solution"
            res <- request "POST" "/solve_captcha" jsonHeaders captchaSolution
            let Just (JsonTop (captchaCorrect :: Bool)) = Aeson.decode (simpleBody res)
            liftIO $ statusCode (simpleStatus res) `shouldBe` 201
            liftIO $ captchaCorrect `shouldBe` True
            liftIO $ rowCountShouldBe connPool "captchas" 0

        it "returns False if an correct solution is posted" $ do
            connPool :: Pool Connection <- liftIO $ readMVar connPoolVar
            void . liftIO $ doTransaction connPool
                [sql| INSERT INTO captchas (id, solution)
                      VALUES ('id', 'solution') |] ()
            let captchaSolution = Aeson.encode $ CaptchaSolution (CaptchaId "id") "wrong"
            res <- request "POST" "/solve_captcha" jsonHeaders captchaSolution
            let Just (JsonTop (captchaCorrect :: Bool)) = Aeson.decode (simpleBody res)
            liftIO $ statusCode (simpleStatus res) `shouldBe` 201
            liftIO $ captchaCorrect `shouldBe` False
            liftIO $ rowCountShouldBe connPool "captchas" 0

jsonHeaders :: [Header]
jsonHeaders = [("Content-Type", "application/json")]
