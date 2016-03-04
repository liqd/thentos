{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Thentos.Backend.Api.CaptchaSpec (spec) where

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, tryTakeMVar, readMVar)
import Data.Configifier (ToConfig, Id, Tagged(Tagged), (>>.))
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Network.Wai (Application)
import Network.Wai.Test (simpleBody, simpleStatus)
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.Status (Status(statusCode))
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec (Spec, SpecWith, describe, it, shouldBe)
import Test.Hspec.Wai (with, request)

import Thentos.Prelude
import Thentos.Action.Types
import Thentos.Backend.Api.Captcha (serveFrontendApi, serveBackendApi)
import Thentos.Config (ThentosConfig)
import Thentos.Test.Core
import Thentos.Test.Transaction
import Thentos.Types

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS

app :: (ThentosConfig -> Maybe (ToConfig cfg Id))
    -> (Tagged cfg -> ActionEnv -> b) -> IO b
app theConfig serveApi = do
    st <- createActionEnv
    void $ tryTakeMVar connPoolVar -- discard old value, if any
    putMVar connPoolVar $ st ^. aStDb
    let Just beConfig = Tagged <$> theConfig (st ^. aStConfig)
    return $! serveApi beConfig st

backendApp :: IO Application
backendApp = app (>>. (Proxy :: Proxy '["backend"])) serveBackendApi

frontendApp :: IO Application
frontendApp = app (>>. (Proxy :: Proxy '["frontend"])) serveFrontendApi

spec :: Spec
spec = describe "Thentos.Backend.Api.Captcha" $ do
        with frontendApp specFrontend
        with backendApp specBackend

connPoolVar :: MVar (Pool Connection)
connPoolVar = unsafePerformIO $ newEmptyMVar
{-# NOINLINE connPoolVar #-}

specFrontend :: SpecWith Application
specFrontend = do
    let f url = do
            res <- request "POST" url [] ""
            liftIO $ statusCode (simpleStatus res) `shouldBe` 201
            liftIO $ LBS.length (simpleBody res) > 0 `shouldBe` True
            connPool :: Pool Connection <- liftIO $ readMVar connPoolVar
            liftIO $ rowCountShouldBe connPool "captchas" 1

    describe "/captcha" $ do
        it "returns a png image on empty POST request" $ f "/captcha"

    describe "/audio_captcha" $ do
        it "returns a sound file on empty POST request" $ f "/audio_captcha/en"

specBackend :: SpecWith Application
specBackend = do
    describe "/solve_captcha" $ do
        let f guess good = do
                let captchaSolution = Aeson.encode $ CaptchaSolution (CaptchaId "id") guess
                res <- request "POST" "/solve_captcha" jsonHeaders captchaSolution
                let Just (JsonTop (captchaCorrect :: Bool)) = Aeson.decode (simpleBody res)
                liftIO $ statusCode (simpleStatus res) `shouldBe` 201
                liftIO $ captchaCorrect `shouldBe` good

            g = do
                connPool :: Pool Connection <- liftIO $ readMVar connPoolVar
                void . liftIO $ doTransaction connPool
                    [sql| INSERT INTO captchas (id, solution)
                          VALUES ('id', 'right') |] ()
                return connPool

            h guess good connPool = do
                f guess good
                liftIO $ rowCountShouldBe connPool "captchas" 0

        it "returns false when the captcha id does not exist" $       f "not" False
        it "returns False if an incorrect solution is posted" $ g >>= h "wrong" False
        it "returns True if the correct solution is posted"   $ g >>= h "right" True

jsonHeaders :: [Header]
jsonHeaders = [("Content-Type", "application/json")]
