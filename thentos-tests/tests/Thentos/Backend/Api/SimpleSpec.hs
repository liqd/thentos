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
{-# LANGUAGE TemplateHaskell                          #-}
{-# LANGUAGE TypeSynonymInstances                     #-}

{-# OPTIONS_GHC #-}

module Thentos.Backend.Api.SimpleSpec (spec, tests)
where

import Control.Lens
import Control.Monad.State (liftIO)
import Control.Monad (void)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Data.Configifier (Tagged(Tagged), (>>.), Source(YamlString))
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (SBS, LBS, ST, cs)
import Network.HTTP.Types.Header (Header)
import Network.Wai (Application)
import Network.Wai.Test (simpleBody, simpleHeaders, SResponse)
import Servant.Docs (toSample)
import System.FilePath ((</>))
import System.Process (readProcess)
import Test.Hspec
    ( Spec, SpecWith, ActionWith, hspec, around, describe, it
    , shouldBe, shouldContain, shouldNotBe )
import Test.Hspec.Wai.Internal (runWaiSession)
import Test.Hspec.Wai
    (shouldRespondWith, WaiSession, with, post, request, matchStatus, pendingWith)
import Text.Email.Validate (unsafeEmailAddress)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as ST

import Thentos.Action.Types
import Thentos.Backend.Api.Simple (serveApi)
import Thentos.Types
import Thentos.Transaction.Core (runThentosQuery)
import Thentos.Transaction (addPersona)
import Thentos (createDefaultUser)

import Thentos.Test.Config
import Thentos.Test.Core
import Thentos.Test.DefaultSpec
import Thentos.Test.Transaction


-- | FIXME: This should be provided in a more general way from "Thentos.Test.Core".
data ItsState = ItsState
    { _itsApplication :: Application
    , _itsActionEnv   :: ActionEnv
    , _itsGodHeader   :: Header
    }

makeLenses ''ItsState

runIt :: (ItsState -> WaiSession a) -> ItsState -> IO a
runIt session its = runWaiSession (session its) (its ^. itsApplication)

withIt :: ActionWith ItsState -> IO ()
withIt action = outsideTempDirectory $ (action =<<) . setupIt . Just

setupIt :: Maybe FilePath -> IO ItsState
setupIt mTmp = do
    let cfgExtra = [ YamlString . cs . unlines $
                        "log:" :
                        "  level: DEBUG" :
                        "  stdout: False" :
                        ["  path: " ++ maybe "/dev/null" (</> "log") mTmp] ]
    cfg <- thentosTestConfig' cfgExtra
    as <- createActionEnv' cfg
    let Just becfg = Tagged <$> cfg >>. (Proxy :: Proxy '["backend"])
        app = serveApi becfg as
    createDefaultUser as
    godHeader <- snd <$> loginAsDefaultUser as
    return $! ItsState app as godHeader

tests :: IO ()
tests = hspec spec

spec :: Spec
spec = describe "Thentos.Backend.Api.Simple" $ do
    with (view itsApplication <$> setupIt Nothing) specHasRestDocs
    around withIt specRest

specRest :: SpecWith ItsState
specRest = do
    describe "headers" $ do
        it "bad unknown headers matching /X-Thentos-*/ yields an error response." . runIt $
          \its -> do
            let headers = [("X-Thentos-No-Such-Header", "3"), jsonHeader, its ^. itsGodHeader]
            request "GET" "/user/0/email" headers "" `shouldRespondWith` 400

    describe "user" $ do
        describe "Capture \"userid\" UserId :> \"name\" :> Get (JsonTop UserName)" $ do
            let resource = "/user/1/name"
            it "yields a name" . runIt $ \its -> do
                let hdr = [jsonHeader, its ^. itsGodHeader]
                request "GET" resource hdr "" `shouldRespondWith` "{\"data\":\"god\"}"

            it "can be called by user herself" . runIt $ \_its -> do
                pendingWith "test missing."

            it "can be called by admin" . runIt $ \its -> do
                let hdr = [jsonHeader, its ^. itsGodHeader]
                request "GET" resource hdr "" `shouldRespondWith` 200

            it "can not be called by other (non-admin) users" . runIt $ \_its -> do
                pendingWith "test missing."

        describe "Capture \"userid\" UserId :> \"email\" :> Get (JsonTop UserEmail)" $ do
            let resource = "/user/1/email"
            it "yields an email address" . runIt $ \its -> do
                let hdr = [jsonHeader, its ^. itsGodHeader]
                request "GET" resource hdr "" `shouldRespondWith` 200

        describe "ReqBody UserFormData :> Post (JsonTop UserId)" $ do
            it "writes a new user to the database" . runIt $ \its -> do
                let hdr = [jsonHeader, its ^. itsGodHeader]
                response1 <- postDefaultUser its
                return response1 `shouldRespondWith` 200

                let Right (uid :: Int) = decodeJsonTop $ simpleBody response1
                response2 <- request "GET" ("/user/" <> (cs . show $ uid) <> "/name") hdr ""

                let Right top2 = decodeJsonTop $ simpleBody response2
                liftIO $ top2 `shouldBe` udName defaultUserData

            it "can only be called by admins" . runIt $ \_its -> do
                pendingWith "test missing."

        describe "Capture \"userid\" UserId :> Delete" $ do
            it "removes an existing user from the database" . runIt $ \its -> do
                let hdr = [jsonHeader, its ^. itsGodHeader]
                response1 <- postDefaultUser its
                let Right (uid :: Int) = decodeJsonTop $ simpleBody response1
                request "GET" ("/user/" <> (cs . show $ uid) <> "/name") hdr ""
                    `shouldRespondWith` 200
                void $ request "DELETE" ("/user/" <> cs (show uid)) hdr ""
                request "GET" ("/user/" <> cs (show uid) <> "/name") hdr ""
                    `shouldRespondWith` 404

            it "can only be called by admins and the user herself" . runIt $ \_its -> do
                pendingWith "test missing."

            it "if user does not exist, responds with a 404" . runIt $ \its -> do
                let hdr = [jsonHeader, its ^. itsGodHeader]
                request "DELETE" "/user/1797" hdr "" `shouldRespondWith` 404

        describe "captcha POST" $ do
            it "returns a PNG image with Thentos-Captcha-Id header" . runIt $ \_its -> do
                rsp <- post "/user/captcha" ""
                pure rsp `shouldRespondWith` 200
                -- Check for magic bytes at start of PNG
                liftIO $ LBS.take 4 (simpleBody rsp) `shouldBe` "\137PNG"
                liftIO $ map fst (simpleHeaders rsp) `shouldContain` ["Thentos-Captcha-Id"]

            it "returns different data on repeated calls" . runIt $ \_its -> do
                rsp1 <- post "/user/captcha" ""
                rsp2 <- post "/user/captcha" ""
                liftIO $ simpleBody rsp1 `shouldNotBe` simpleBody rsp2

        let getCaptchaAndSolution :: ItsState -> WaiSession (SBS, ST)
            getCaptchaAndSolution its = do
                crsp <- post "/user/captcha" ""
                let Just cid = lookup "Thentos-Captcha-Id" $ simpleHeaders crsp
                [Only solution] <- liftIO $ doQuery (its ^. itsActionEnv . aStDb)
                    [sql| SELECT solution FROM captchas WHERE id = ? |] (Only cid)
                return (cid, solution)

        describe "register POST" $ do
            let grepLogFile :: ItsState -> String -> WaiSession String
                grepLogFile its line = liftIO $ do
                    let logFile :: FilePath
                        logFile = cs $ (its ^. itsActionEnv . aStConfig)
                            >>. (Proxy :: Proxy '["log", "path"])
                    readProcess "grep" [line, logFile] ""

            it "responds with 200 and sends mail with confirmation token" . runIt $
              \its -> do
                (cid, solution) <- getCaptchaAndSolution its
                -- Register user
                let csol    = CaptchaSolution (CaptchaId $ cs cid) solution
                    reqBody = Aeson.encode $ UserCreationRequest defaultUserData csol
                request "POST" "/user/register" [jsonHeader] reqBody `shouldRespondWith` 200
                -- Find token in sent email and make sure it's correct
                let actPrefix = "/activate/"
                actLine <- grepLogFile its actPrefix
                let sentToken = ST.take 24 . snd $ ST.breakOnEnd (cs actPrefix) (cs actLine)
                [Only (actualTok :: ConfirmationToken)] <-
                    liftIO $ doQuery (its ^. itsActionEnv . aStDb)
                        [sql| SELECT token FROM user_confirmation_tokens |] ()
                liftIO $ sentToken `shouldBe` fromConfirmationToken actualTok

            it "responds with 200 and sends warn mail if email is duplicate" . runIt $
              \its -> do
                (cid, solution) <- getCaptchaAndSolution its
                -- Create user
                void $ postDefaultUser its
                -- Try to register another user with the same email
                let csol    = CaptchaSolution (CaptchaId $ cs cid) solution
                    user    = UserFormData "Another" "password" (udEmail defaultUserData)
                    reqBody = Aeson.encode $ UserCreationRequest user csol
                request "POST" "/user/register" [jsonHeader] reqBody `shouldRespondWith` 200
                -- Check that no confirmation token was generated
                liftIO $ rowCountShouldBe (its ^. itsActionEnv . aStDb) "user_confirmation_tokens" 0
                -- Check that "Attempted Signup" mail was sent
                actLine <- grepLogFile its "Thentos: Attempted Signup"
                liftIO $ actLine `shouldNotBe` ""

            it "refuses to accept the correct solution to the same captcha twice" . runIt $
              \its -> do
                (cid, solution) <- getCaptchaAndSolution its
                -- Register user
                let csol    = CaptchaSolution (CaptchaId $ cs cid) solution
                    reqBody = Aeson.encode $ UserCreationRequest defaultUserData csol
                request "POST" "/user/register" [jsonHeader] reqBody `shouldRespondWith` 200
                -- Try to register another user
                let user2 = UserFormData "name2" "password" $ forceUserEmail "another@example.org"
                    reqBody2 = Aeson.encode $ UserCreationRequest user2 csol
                request "POST" "/user/register" [jsonHeader] reqBody2 `shouldRespondWith` 400

            it "allows resubmitting the same captcha solution if the user name was not unique" .
              runIt $ \its -> do
                (cid, solution) <- getCaptchaAndSolution its
                -- Create user
                void $ postDefaultUser its
                -- Try to register another user with the same name
                let csol    = CaptchaSolution (CaptchaId $ cs cid) solution
                    user    = UserFormData (udName defaultUserData) "password" $
                                           forceUserEmail "another@example.org"
                    reqBody = Aeson.encode $ UserCreationRequest user csol
                request "POST" "/user/register" [jsonHeader] reqBody `shouldRespondWith` 403
                -- Try again using a new user name
                let user2    = user { udName = "newname" }
                    reqBody2 = Aeson.encode $ UserCreationRequest user2 csol
                request "POST" "/user/register" [jsonHeader] reqBody2 `shouldRespondWith` 200

            it "fails if called without correct captcha ID" . runIt $ \_its -> do
                let csol    = CaptchaSolution "no-such-id" "dummy"
                    reqBody = Aeson.encode $ UserCreationRequest defaultUserData csol
                request "POST" "/user/register" [jsonHeader] reqBody `shouldRespondWith` 400

            it "fails if called without correct captcha solution" . runIt $ \_its -> do
                crsp <- post "/user/captcha" ""
                let Just cid = lookup "Thentos-Captcha-Id" $ simpleHeaders crsp
                let csol    = CaptchaSolution (CaptchaId $ cs cid) "probably wrong"
                    reqBody = Aeson.encode $ UserCreationRequest defaultUserData csol
                request "POST" "/user/register" [jsonHeader] reqBody `shouldRespondWith` 400

            it "fails if the user's password is too short" . runIt $ \its -> do
                (cid, solution) <- getCaptchaAndSolution its
                let csol     = CaptchaSolution (CaptchaId $ cs cid) solution
                    userData = defaultUserData { udPassword = "short" }
                    reqBody  = Aeson.encode $ UserCreationRequest userData csol
                request "POST" "/user/register" [jsonHeader] reqBody `shouldRespondWith` 400

        -- Note: this code assumes that there is just one unconfirmed user in the DB.
        let registerUserAndGetConfirmationToken :: ItsState -> (SBS, ST)
                                                 -> WaiSession ConfirmationToken
            registerUserAndGetConfirmationToken its (cid, solution) = do
                -- Register user and get confirmation token
                let csol     = CaptchaSolution (CaptchaId $ cs cid) solution
                    rreqBody = Aeson.encode $ UserCreationRequest defaultUserData csol
                void $ request "POST" "/user/register" [jsonHeader] rreqBody
                -- There should be just one token in the DB
                [Only (confTok :: ConfirmationToken)] <-
                    liftIO $ doQuery (its ^. itsActionEnv . aStDb)
                    [sql| SELECT token FROM user_confirmation_tokens |] ()
                return confTok

        describe "activate POST" $ do
            it "activates a new user" . runIt $ \its -> do
                (cid, solution) <- getCaptchaAndSolution its
                confTok <- registerUserAndGetConfirmationToken its (cid, solution)
                -- Activate user
                let areqBody = Aeson.encode $ JsonTop confTok
                arsp <- request "POST" "/user/activate" [jsonHeader] areqBody
                pure arsp `shouldRespondWith` 200
                let Right (sessTok :: ThentosSessionToken) = decodeJsonTop $ simpleBody arsp
                liftIO $ fromThentosSessionToken sessTok `shouldNotBe` ""

            it "fails if called again" . runIt $ \its -> do
                (cid, solution) <- getCaptchaAndSolution its
                confTok <- registerUserAndGetConfirmationToken its (cid, solution)
                -- Activate user
                let areqBody = Aeson.encode $ JsonTop confTok
                request "POST" "/user/activate" [jsonHeader] areqBody `shouldRespondWith` 200
                -- Try to activate again
                request "POST" "/user/activate" [jsonHeader] areqBody `shouldRespondWith` 400

            it "fails if called without valid ConfirmationToken" . runIt $ \_its -> do
                let reqBody = Aeson.encode . JsonTop $ ConfirmationToken "no-such-token"
                request "POST" "/user/activate" [jsonHeader] reqBody `shouldRespondWith` 400

        describe "login POST" $ do
            it "logs an activated user in" . runIt $ \its -> do
                (cid, solution) <- getCaptchaAndSolution its
                confTok <- registerUserAndGetConfirmationToken its (cid, solution)
                -- Activate user
                let areqBody = Aeson.encode $ JsonTop confTok
                request "POST" "/user/activate" [jsonHeader] areqBody `shouldRespondWith` 200
                -- Log them in
                let loginData = LoginFormData (udName defaultUserData) (udPassword defaultUserData)
                lrsp <- request "POST" "/user/login" [jsonHeader] $ Aeson.encode loginData
                pure lrsp `shouldRespondWith` 200
                let Right (sessTok :: ThentosSessionToken) = decodeJsonTop $ simpleBody lrsp
                liftIO $ fromThentosSessionToken sessTok `shouldNotBe` ""

            it "fails in the same way if user doesn't exist or password is wrong" . runIt $ \its -> do
                void $ postDefaultUser its
                let loginData1 = LoginFormData "wrong name" (udPassword defaultUserData)
                rsp1 <- request "POST" "/user/login" [jsonHeader] $ Aeson.encode loginData1
                pure rsp1 `shouldRespondWith` 401
                let loginData2 = LoginFormData (udName defaultUserData) "wrong pass"
                rsp2 <- request "POST" "/user/login" [jsonHeader] $ Aeson.encode loginData2
                pure rsp2 `shouldRespondWith` 401
                liftIO $ simpleBody rsp1 `shouldBe` simpleBody rsp2

            it "fails if user isn't yet activated" . runIt $ \its -> do
                (cid, solution) <- getCaptchaAndSolution its
                -- Register user
                let csol    = CaptchaSolution (CaptchaId $ cs cid) solution
                    rreqBody = Aeson.encode $ UserCreationRequest defaultUserData csol
                void $ request "POST" "/user/register" [jsonHeader] rreqBody
                -- Try to log them in
                let loginData = LoginFormData (udName defaultUserData) (udPassword defaultUserData)
                request "POST" "/user/login" [jsonHeader] (Aeson.encode loginData)
                    `shouldRespondWith` 401


    describe "thentos_session" $ do
        describe "ReqBody '[JSON] ThentosSessionToken :> Get Bool" $ do
            it "returns true if session is active" . runIt $ \its -> do
                let hdr = [jsonHeader, its ^. itsGodHeader]
                response1 <- postDefaultUser its
                let Right uid = decodeJsonTop $ simpleBody response1
                response2 <- request "POST" "/thentos_session" hdr $
                    Aeson.encode $ ByUser (UserId uid) (udPassword defaultUserData)
                request "GET" "/thentos_session/" hdr (simpleBody response2)
                    `shouldRespondWith` "true" { matchStatus = 200 }

            it "returns false if session is does not exist" . runIt $ \its -> do
                void $ postDefaultUser its
                let hdr = [jsonHeader, its ^. itsGodHeader]
                request "GET" "/thentos_session/" hdr (Aeson.encode ("x" :: ThentosSessionToken))
                    `shouldRespondWith` "false" { matchStatus = 200 }


    describe "email" $ do
        describe "ReqBody '[JSON] SendEmailRequest :> Post '[JSON] ()" $ do
            let sendEmail emails personas = do
                    let hdr  = [jsonHeader]
                        recp = EmailRecipients emails personas
                        subj = "Email subject"
                        body = "Email body"
                    request "POST" "/email" hdr (Aeson.encode $ SendEmailRequest recp subj body Nothing)
                Just uri = toSample (Proxy :: Proxy Uri)
            it "sends an email to an explicit email address." . runIt $ \_its -> do
                sendEmail [udEmail defaultUserData] [] `shouldRespondWith` "[]" { matchStatus = 200 }
            it "sends an email to a persona id." . runIt $ \its -> do
                let connPool = its ^. itsActionEnv . aStDb
                (uid, _, _):_ <- createTestUsers connPool 2
                Right _persona <- liftIO $ runThentosQuery connPool $ addPersona persName uid (Just uri)
                sendEmail [] [uri] `shouldRespondWith` "[]" { matchStatus = 200 }
            it "fails if the email is malformed." . runIt $ \_its ->
                sendEmail [malformedUserEmail] [] `shouldRespondWith` 400
            it "fails if the persona does not exist." . runIt $ \_its ->
                sendEmail [] [uri] `shouldRespondWith` 400
            it "can only be called from privileged IPs." . runIt $ \_its -> do
                pendingWith "test missing."


-- | Parse and unwrap an element wrapped in JsonTop.
decodeJsonTop :: Aeson.FromJSON a => LBS -> Either String a
decodeJsonTop bs = fromJsonTop <$> Aeson.eitherDecode bs

postDefaultUser :: ItsState -> WaiSession SResponse
postDefaultUser its = do
    request "POST" "/user" [jsonHeader, its ^. itsGodHeader] (Aeson.encode defaultUserData)

-- | Set content type to JSON.
jsonHeader :: Header
jsonHeader = ("Content-Type", "application/json")

defaultUserData :: UserFormData
defaultUserData = UserFormData "name" "PASSword" $ forceUserEmail "somebody@example.org"

malformedUserEmail :: UserEmail
malformedUserEmail = UserEmail $ unsafeEmailAddress "wrong" ""
