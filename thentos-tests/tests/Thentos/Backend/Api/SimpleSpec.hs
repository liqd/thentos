{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE ExistentialQuantification                #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE InstanceSigs                             #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE QuasiQuotes                              #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TupleSections                            #-}
{-# LANGUAGE TypeSynonymInstances                     #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Thentos.Backend.Api.SimpleSpec (spec, tests)
where

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar, tryTakeMVar)
import Control.Monad.State (liftIO)
import Control.Monad (void)
import Data.Configifier (Tagged(Tagged), (>>.))
import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import Data.Monoid ((<>))
import Data.Pool (Pool, withResource)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (SBS, LBS, ST, cs)
import Database.PostgreSQL.Simple (Connection, Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.Status (statusCode)
import Network.Wai (Application)
import Network.Wai.Test (simpleBody, simpleHeaders, simpleStatus, SResponse)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)
import Test.Hspec (Spec, SpecWith, around_, describe, it, shouldBe, shouldContain, shouldNotBe,
                   pendingWith, hspec)
import Test.Hspec.Wai (shouldRespondWith, WaiSession, with, request, matchStatus)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as ST

import Thentos.Action.Core
import Thentos.Backend.Api.Simple (serveApi)
import Thentos.Types

import Thentos.Test.Config
import Thentos.Test.Core
import Thentos.Test.DefaultSpec
import Thentos.Test.Transaction


-- | FIXME: This is stuff also provided in a similar way from "Thentos.Test.Core".  remove redundancy!
defaultApp :: IO Application
defaultApp = do
    as@(ActionState (connPool, _, cfg)) <- createActionState "test_thentos" thentosTestConfig
    withResource connPool createGod
    writeIORef godHeaders . snd =<< loginAsGod as
    void $ tryTakeMVar connPoolVar  -- discard old value, if any
    putMVar connPoolVar connPool
    let Just beConfig = Tagged <$> cfg >>. (Proxy :: Proxy '["backend"])
    return $! serveApi beConfig as

tests :: IO ()
tests = hspec spec

godHeaders :: IORef [Header]
godHeaders = unsafePerformIO $ newIORef []
{-# NOINLINE godHeaders #-}

connPoolVar :: MVar (Pool Connection)
connPoolVar = unsafePerformIO $ newEmptyMVar
{-# NOINLINE connPoolVar #-}

spec :: Spec
spec = describe "Thentos.Backend.Api.Simple" $ with defaultApp specRest

specRest :: SpecWith Application
specRest = do
    specHasRestDocs

    describe "headers" $ do
        it "bad unknown headers matching /X-Thentos-*/ yields an error response." $ do
            hdr <- liftIO ctHeader
            let headers = ("X-Thentos-No-Such-Header", "3"):hdr
            request "GET" "/user/0/email" headers "" `shouldRespondWith` 400


    describe "user" $ do
        describe "Capture \"userid\" UserId :> \"name\" :> Get (JsonTop UserName)" $ do
            let resource = "/user/0/name"
            it "yields a name" $ do
                hdr <- liftIO ctHeader
                request "GET" resource hdr "" `shouldRespondWith` "{\"data\":\"god\"}"

            it "can be called by user herself" $
                    \ _ -> pendingWith "test missing."

            it "can be called by admin" $ do
                hdr <- liftIO ctHeader
                request "GET" resource hdr "" `shouldRespondWith` 200

            it "can not be called by other (non-admin) users" $
                    \ _ -> pendingWith "test missing."

        describe "Capture \"userid\" UserId :> \"email\" :> Get (JsonTop UserEmail)" $ do
            let resource = "/user/0/email"
            it "yields an email address" $ do
                hdr <- liftIO ctHeader
                request "GET" resource hdr "" `shouldRespondWith` 200

        describe "ReqBody UserFormData :> Post (JsonTop UserId)" $ do
            it "writes a new user to the database" $ do
                hdr <- liftIO ctHeader
                response1 <- postDefaultUser
                return response1 `shouldRespondWith` 201

                let Right (uid :: Int) = decodeJsonTop $ simpleBody response1
                response2 <- request "GET" ("/user/" <> (cs . show $ uid) <> "/name") hdr ""

                let Right top2 = decodeJsonTop $ simpleBody response2
                liftIO $ top2 `shouldBe` udName defaultUserData

            it "can only be called by admins" $
                    \ _ -> pendingWith "test missing."

        describe "Capture \"userid\" UserId :> Delete" $ do
            it "removes an existing user from the database" $ do
                hdr <- liftIO ctHeader
                response1 <- postDefaultUser
                let Right (uid :: Int) = decodeJsonTop $ simpleBody response1
                request "GET" ("/user/" <> (cs . show $ uid) <> "/name") hdr ""
                    `shouldRespondWith` 200
                void $ request "DELETE" ("/user/" <> cs (show uid)) hdr ""
                request "GET" ("/user/" <> cs (show uid) <> "/name") hdr ""
                    `shouldRespondWith` 404

            it "can only be called by admins and the user herself" $
                    \ _ -> pendingWith "test missing."

            it "if user does not exist, responds with a 404" $ do
                hdr <- liftIO ctHeader
                request "DELETE" "/user/1797" hdr "" `shouldRespondWith` 404

        describe "captcha POST" $ do
            it "returns a PNG image with Thentos-Captcha-Id header" $ do
                rsp <- request "POST" "/user/captcha" [] ""
                liftIO $ statusCode (simpleStatus rsp) `shouldBe` 201
                -- Check for magic bytes at start of PNG
                liftIO $ LBS.take 4 (simpleBody rsp) `shouldBe` "\137PNG"
                liftIO $ map fst (simpleHeaders rsp) `shouldContain` ["Thentos-Captcha-Id"]

            it "returns different data on repeated calls" $ do
                rsp1 <- request "POST" "/user/captcha" [] ""
                rsp2 <- request "POST" "/user/captcha" [] ""
                liftIO $ simpleBody rsp1 `shouldNotBe` simpleBody rsp2

        let getCaptchaAndSolution :: WaiSession (SBS, ST)
            getCaptchaAndSolution = do
                crsp <- request "POST" "/user/captcha" [] ""
                let Just cid = lookup "Thentos-Captcha-Id" $ simpleHeaders crsp
                connPool :: Pool Connection <- liftIO $ readMVar connPoolVar
                [Only solution] <- liftIO $ doQuery connPool
                    [sql| SELECT solution FROM captchas WHERE id = ? |] (Only cid)
                return (cid, solution)

        describe "register POST" $ do
            around_ withLogger $ do
                it "responds with 204 No Content and sends mail with confirmation token" $ do
                    (cid, solution) <- getCaptchaAndSolution
                    -- Register user
                    let csol    = CaptchaSolution (CaptchaId $ cs cid) solution
                        reqBody = Aeson.encode $ UserCreationRequest defaultUserData csol
                    request "POST" "/user/register" jsonHeader reqBody `shouldRespondWith` 204
                    -- Find token in sent email and make sure it's correct
                    let actPrefix = "/activate/"
                    actLine <- liftIO $ readProcess "grep" [actPrefix, "everything.log"] ""
                    let sentToken = ST.take 24 . snd $ ST.breakOnEnd (cs actPrefix) (cs actLine)
                    connPool :: Pool Connection <- liftIO $ readMVar connPoolVar
                    [Only (actualTok :: ConfirmationToken)] <- liftIO $ doQuery connPool
                        [sql| SELECT token FROM user_confirmation_tokens |] ()
                    liftIO $ sentToken `shouldBe` fromConfirmationToken actualTok

                it "responds with 204 No Content and sends warn mail if email is duplicate" $ do
                    (cid, solution) <- getCaptchaAndSolution
                    -- Create user
                    void postDefaultUser
                    -- Try to register another user with the same email
                    let csol    = CaptchaSolution (CaptchaId $ cs cid) solution
                        user    = UserFormData "Another" "pwd" (udEmail defaultUserData)
                        reqBody = Aeson.encode $ UserCreationRequest user csol
                    request "POST" "/user/register" jsonHeader reqBody `shouldRespondWith` 204
                    -- Check that no confirmation token was generated
                    connPool :: Pool Connection <- liftIO $ readMVar connPoolVar
                    liftIO $ rowCountShouldBe connPool "user_confirmation_tokens" 0
                    -- Check that "Attempted Signup" mail was sent
                    actLine <- liftIO $
                        readProcess "grep" ["Thentos: Attempted Signup", "everything.log"] ""
                    liftIO $ actLine `shouldNotBe` ""

            it "refuses to accept the correct solution to the same captcha twice" $ do
                (cid, solution) <- getCaptchaAndSolution
                -- Register user
                let csol    = CaptchaSolution (CaptchaId $ cs cid) solution
                    reqBody = Aeson.encode $ UserCreationRequest defaultUserData csol
                request "POST" "/user/register" jsonHeader reqBody `shouldRespondWith` 204
                -- Try to register another user
                let user2    = UserFormData "name2" "pwd" $ forceUserEmail "another@example.org"
                    reqBody2 = Aeson.encode $ UserCreationRequest user2 csol
                request "POST" "/user/register" jsonHeader reqBody2 `shouldRespondWith` 400

            it "allows resubmitting the same captcha solution if the user name was not unique" $ do
                (cid, solution) <- getCaptchaAndSolution
                -- Create user
                void postDefaultUser
                -- Try to register another user with the same name
                let csol    = CaptchaSolution (CaptchaId $ cs cid) solution
                    user    = UserFormData (udName defaultUserData) "pwd" $
                                           forceUserEmail "another@example.org"
                    reqBody = Aeson.encode $ UserCreationRequest user csol
                request "POST" "/user/register" jsonHeader reqBody `shouldRespondWith` 403
                -- Try again using a new user name
                let user2    = user { udName = "newname" }
                    reqBody2 = Aeson.encode $ UserCreationRequest user2 csol
                request "POST" "/user/register" jsonHeader reqBody2 `shouldRespondWith` 204

            it "fails if called without correct captcha ID" $ do
                let csol    = CaptchaSolution "no-such-id" "dummy"
                    reqBody = Aeson.encode $ UserCreationRequest defaultUserData csol
                request "POST" "/user/register" jsonHeader reqBody `shouldRespondWith` 400

            it "fails if called without correct captcha solution" $ do
                crsp <- request "POST" "/user/captcha" [] ""
                let Just cid = lookup "Thentos-Captcha-Id" $ simpleHeaders crsp
                let csol    = CaptchaSolution (CaptchaId $ cs cid) "probably wrong"
                    reqBody = Aeson.encode $ UserCreationRequest defaultUserData csol
                request "POST" "/user/register" jsonHeader reqBody `shouldRespondWith` 400

        -- Note: this code assumes that there is just one unconfirmed user in the DB.
        let registerUserAndGetConfirmationToken :: (SBS, ST) -> WaiSession ConfirmationToken
            registerUserAndGetConfirmationToken (cid, solution) = do
                -- Register user and get confirmation token
                let csol     = CaptchaSolution (CaptchaId $ cs cid) solution
                    rreqBody = Aeson.encode $ UserCreationRequest defaultUserData csol
                void $ request "POST" "/user/register" jsonHeader rreqBody
                connPool :: Pool Connection <- liftIO $ readMVar connPoolVar
                -- There should be just one token in the DB
                [Only (confTok :: ConfirmationToken)] <- liftIO $ doQuery connPool
                    [sql| SELECT token FROM user_confirmation_tokens |] ()
                return confTok

        describe "activate POST" $ do
            it "activates a new user" $ do
                (cid, solution) <- getCaptchaAndSolution
                confTok <- registerUserAndGetConfirmationToken (cid, solution)
                -- Activate user
                let areqBody = Aeson.encode $ JsonTop confTok
                arsp <- request "POST" "/user/activate" jsonHeader areqBody
                liftIO $ statusCode (simpleStatus arsp) `shouldBe` 201
                let Right (sessTok :: ThentosSessionToken) = decodeJsonTop $ simpleBody arsp
                liftIO $ fromThentosSessionToken sessTok `shouldNotBe` ""

            it "fails if called again" $ do
                (cid, solution) <- getCaptchaAndSolution
                confTok <- registerUserAndGetConfirmationToken (cid, solution)
                -- Activate user
                let areqBody = Aeson.encode $ JsonTop confTok
                arsp <- request "POST" "/user/activate" jsonHeader areqBody
                liftIO $ statusCode (simpleStatus arsp) `shouldBe` 201
                -- Try to activate again
                request "POST" "/user/activate" jsonHeader areqBody `shouldRespondWith` 400

            it "fails if called without valid ConfirmationToken" $ do
                let reqBody = Aeson.encode . JsonTop $ ConfirmationToken "no-such-token"
                request "POST" "/user/activate" jsonHeader reqBody `shouldRespondWith` 400

        describe "login POST" $ do
            it "logs an activated user in" $ do
                (cid, solution) <- getCaptchaAndSolution
                confTok <- registerUserAndGetConfirmationToken (cid, solution)
                -- Activate user
                let areqBody = Aeson.encode $ JsonTop confTok
                arsp <- request "POST" "/user/activate" jsonHeader areqBody
                liftIO $ statusCode (simpleStatus arsp) `shouldBe` 201
                -- Log them in
                let loginData = LoginFormData (udName defaultUserData) (udPassword defaultUserData)
                lrsp <- request "POST" "/user/login" jsonHeader $ Aeson.encode loginData
                liftIO $ statusCode (simpleStatus lrsp) `shouldBe` 201
                let Right (sessTok :: ThentosSessionToken) = decodeJsonTop $ simpleBody lrsp
                liftIO $ fromThentosSessionToken sessTok `shouldNotBe` ""

            it "fails in the same way if user doesn't exist or password is wrong" $ do
                void postDefaultUser
                let loginData1 = LoginFormData "wrong name" (udPassword defaultUserData)
                rsp1 <- request "POST" "/user/login" jsonHeader $ Aeson.encode loginData1
                liftIO $ statusCode (simpleStatus rsp1) `shouldBe` 401
                let loginData2 = LoginFormData (udName defaultUserData) "wrong pass"
                rsp2 <- request "POST" "/user/login" jsonHeader $ Aeson.encode loginData2
                liftIO $ statusCode (simpleStatus rsp2) `shouldBe` 401
                liftIO $ simpleBody rsp1 `shouldBe` simpleBody rsp2

            it "fails if user isn't yet activated" $ do
                (cid, solution) <- getCaptchaAndSolution
                -- Register user
                let csol    = CaptchaSolution (CaptchaId $ cs cid) solution
                    rreqBody = Aeson.encode $ UserCreationRequest defaultUserData csol
                void $ request "POST" "/user/register" jsonHeader rreqBody
                -- Try to log them in
                let loginData = LoginFormData (udName defaultUserData) (udPassword defaultUserData)
                lrsp <- request "POST" "/user/login" jsonHeader $ Aeson.encode loginData
                liftIO $ statusCode (simpleStatus lrsp) `shouldBe` 401


    describe "thentos_session" $ do
        describe "ReqBody '[JSON] ThentosSessionToken :> Get Bool" $ do
            it "returns true if session is active" $ do
                hdr <- liftIO ctHeader
                response1 <- postDefaultUser
                let Right uid = decodeJsonTop $ simpleBody response1
                response2 <- request "POST" "/thentos_session" hdr $
                    Aeson.encode $ ByUser (UserId uid, udPassword defaultUserData)
                request "GET" "/thentos_session/" hdr (simpleBody response2)
                    `shouldRespondWith` "true" { matchStatus = 200 }

            it "returns false if session is does not exist" $ do
                void postDefaultUser
                hdr <- liftIO ctHeader
                request "GET" "/thentos_session/" hdr (Aeson.encode ("x" :: ThentosSessionToken))
                    `shouldRespondWith` "false" { matchStatus = 200 }


-- | Parse and unwrap an element wrapped in JsonTop. Returns the element, if parsing is successful,
decodeJsonTop :: Aeson.FromJSON a => LBS -> Either String a
decodeJsonTop bs = fromJsonTop <$> Aeson.eitherDecode bs

postDefaultUser :: WaiSession SResponse
postDefaultUser = do
    hdr <- liftIO ctHeader
    request "POST" "/user" hdr (Aeson.encode defaultUserData)

-- | Set content type to JSON.
jsonHeader :: [Header]
jsonHeader = [("Content-Type", "application/json")]

-- | God Headers plus content-type = json
ctHeader :: IO [Header]
ctHeader = (("Content-Type", "application/json") :) <$> readIORef godHeaders

defaultUserData :: UserFormData
defaultUserData = UserFormData "name" "pwd" $ forceUserEmail "somebody@example.org"
