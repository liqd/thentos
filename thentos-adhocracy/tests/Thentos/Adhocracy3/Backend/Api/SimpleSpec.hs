{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ImplicitParams            #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

{-# OPTIONS_GHC -fno-warn-orphans      #-}

module Thentos.Adhocracy3.Backend.Api.SimpleSpec
where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson (Value(String), object, (.=))
import Data.Configifier (Source(YamlString), Tagged(Tagged), (>>.))
import Data.Foldable (for_)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (LBS, ST, cs)
import GHC.Stack (CallStack)
import LIO.DCLabel (toCNF)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Types (Header, Status, status200, status400)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (defaultSettings, setHost, setPort, runSettings)
import Network.Wai.Test (simpleBody, simpleStatus)
import System.FilePath ((</>))
import System.Process (readProcess)
import Test.Hspec (ActionWith, Spec, around_, around, describe, hspec, it,
                   shouldBe, shouldContain, shouldSatisfy, pendingWith)
import Test.Hspec.Wai (request, with)
import Test.Hspec.Wai.Internal (WaiSession, runWaiSession)
import Test.QuickCheck (property)

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Text as ST
import qualified Network.HTTP.Types.Status as Status

import Thentos
import Thentos.Action.Core
import Thentos.Action.Types
import Thentos.Adhocracy3.Action.Types
import Thentos.Adhocracy3.Backend.Api.Simple
import Thentos.Config (ThentosConfig)
import Thentos.Types

import Thentos.Test.Arbitrary ()
import Thentos.Test.Config
import Thentos.Test.Core
import Thentos.Test.Network
import Thentos.Test.DefaultSpec


tests :: IO ()
tests = hspec spec

spec :: Spec
spec =
    describe "Thentos.Backend.Api.Adhocracy3" $ do
        with setupBackend specHasRestDocs

        describe "A3UserNoPass" $ do
            it "has invertible *JSON instances" . property $
                let (===) (Right (A3UserNoPass (UserFormData n _ e)))
                          (Right (A3UserNoPass (UserFormData n' _ e')))
                              = n == n' && e == e'
                    (===) _ _ = False
                in \ (A3UserNoPass -> u) -> (Aeson.eitherDecode . Aeson.encode) u === Right u

        describe "A3UserWithPassword" $ do
            it "has invertible *JSON instances" . property $
                \ (A3UserWithPass -> u) -> (Aeson.eitherDecode . Aeson.encode) u == Right u

            it "rejects short passwords" $ do
                 let userdata = mkUserJson "Anna Müller" "anna@example.org" "short"
                 fromA3UserWithPass <$>
                     (Aeson.eitherDecode userdata :: Either String A3UserWithPass)
                     `shouldBe` Left "password too short (less than 6 characters)"

            it "rejects long passwords" $ do
                 let longpass = ST.replicate 26 "long"
                     userdata = mkUserJson "Anna Müller" "anna@example.org" longpass
                 fromA3UserWithPass <$>
                     (Aeson.eitherDecode userdata :: Either String A3UserWithPass)
                     `shouldBe` Left "password too long (more than 100 characters)"

            it "rejects invalid email addresses" $ do
                 let userdata = mkUserJson "Anna Müller" "anna@" "EckVocUbs3"
                 fromA3UserWithPass <$>
                     (Aeson.eitherDecode userdata :: Either String A3UserWithPass)
                     `shouldBe` Left "expected UserEmail, encountered String \"anna@\""

            it "rejects empty user names" $ do
                 let userdata = mkUserJson "" "anna@example.org" "EckVocUbs3"
                 fromA3UserWithPass <$>
                     (Aeson.eitherDecode userdata :: Either String A3UserWithPass)
                     `shouldBe` Left "user name is empty"

            it "rejects user names with @" $ do
                 let userdata = mkUserJson "Bla@Blub" "anna@example.org" "EckVocUbs3"
                 fromA3UserWithPass <$>
                     (Aeson.eitherDecode userdata :: Either String A3UserWithPass)
                     `shouldBe` Left "'@' in user name is not allowed: \"Bla@Blub\""

            it "rejects user names with too much whitespace" $ do
                 let userdata = mkUserJson " Anna  Toll" "anna@example.org" "EckVocUbs3"
                 fromA3UserWithPass <$>
                     (Aeson.eitherDecode userdata :: Either String A3UserWithPass)
                     `shouldBe` Left "Illegal whitespace sequence in user name: \" Anna  Toll\""

        describe "create user" $ do
            let a3userCreated   = encodePretty $ object
                    [ "content_type" .= String "adhocracy_core.resources.principal.IUser"
                    , "path"         .= String "http://127.0.0.1:6541/principals/users/0000111/"
                    ]
                smartA3backend = routingReplyServer Nothing $ Map.fromList
                    [ ("/path/principals/users", (status200, a3userCreated)) ]

            around (withTestState smartA3backend) $ do
                it "works" . runTestState $ \(_, cfg) -> do
                    let name    = "Anna Müller"
                        pass    = "EckVocUbs3"
                        reqBody = mkUserJson name "anna@example.org" pass
                    -- Appending trailing newline since servant-server < 0.4.1 couldn't handle it
                    rsp <- request "POST" "/principals/users" [ctJson] $ reqBody  <> "\n"
                    shouldBeStatusXWithCustomMessages 200 (simpleStatus rsp) (simpleBody rsp)
                        -- The returned path is now just a dummy that uses our endpoint prefix
                        -- (not the one from A3)
                        ["http://127.0.0.1:7118/"]

                        -- FIXME: we should change Thentos.Test.Config.{back,front}end to so that we
                        -- can distinguish between bind url and exposed url.  i think this here
                        -- needs to be the exposed url.

                    -- Find the confirmation token. We use a system call to "grep" it ouf the
                    -- log file, which is ugly but works, while reading the log file within
                    -- Haskell doesn't (openFile: resource busy (file is locked)).
                    let actPrefix = ":7119/activate/"
                        logPath = cs $ cfg >>. (Proxy :: Proxy '["log", "path"])
                    actLine <- liftIO $ readProcess "grep" [actPrefix, logPath] ""
                    let confToken = ST.take 24 . snd $ ST.breakOnEnd (cs actPrefix) (cs actLine)

                    -- Make sure that we cannot log in since the account isn't yet active
                    let loginReq = mkLoginRequest name pass
                    loginRsp <- request "POST" "login_username" [ctJson] loginReq
                    liftIO $ Status.statusCode (simpleStatus loginRsp) `shouldBe` 400

                    -- Activate user
                    let actReq = encodePretty $ object
                            [ "path" .= String ("/activate/" <> confToken) ]
                    actRsp <- request "POST" "activate_account" [ctJson] actReq
                    liftIO $ Status.statusCode (simpleStatus actRsp) `shouldBe` 200

                    -- Make sure that we can log in now
                    loginRsp2 <- request "POST" "login_username" [ctJson] loginReq
                    liftIO $ Status.statusCode (simpleStatus loginRsp2) `shouldBe` 200

        describe "activate_account" $ with setupBackend $ do
            let a3errMsg = encodePretty $ A3ErrorMessage
                    [A3Error "path" "body" "Unknown or expired activation path"]
            around_ (withA3fake (Just status400) a3errMsg) $ do
                it "rejects bad path mimicking A3" $ do
                    let reqBody = encodePretty $ object
                            [ "path" .= String "/activate/no-such-path" ]
                    rsp <- request "POST" "activate_account" [ctJson] reqBody
                    shouldBeErr400WithCustomMessage (simpleStatus rsp) (simpleBody rsp)
                        "\"Unknown or expired activation path\""

        describe "login" $ with setupBackend $
            it "rejects bad credentials mimicking A3" $ do
                let reqBody = mkLoginRequest "Anna Müller" "this-is-wrong"
                rsp <- request "POST" "login_username" [ctJson] reqBody
                shouldBeErr400WithCustomMessage (simpleStatus rsp) (simpleBody rsp)
                    "\"User doesn't exist or password is wrong\""

        describe "resetPassword" $ with setupBackend $ do
            let a3loginSuccess = encodePretty $ object
                    [ "status"     .= String "success"
                    , "user_path"  .= String "http://127.0.0.1:7118/principals/users/0000000/"
                    , "user_token" .= String "bla-bla-valid-token-blah"
                    ]
            around_ (withA3fake Nothing a3loginSuccess) $ do
                it "changes the password if A3 signals success" $ do

                    liftIO $ pendingWith "see BUG #321"

                    let resetReq = mkPwResetRequestJson "/principals/resets/dummypath" "newpass"
                    rsp <- request "POST" "password_reset" [ctJson] resetReq
                    liftIO $ Status.statusCode (simpleStatus rsp) `shouldBe` 200
                    -- Test that we can log in using the new password
                    let loginReq = mkLoginRequest "god" "newpass"
                    loginRsp <- request "POST" "login_username" [ctJson] loginReq
                    liftIO $ Status.statusCode (simpleStatus loginRsp) `shouldBe` 200

            let a3errMsg = encodePretty $ A3ErrorMessage
                           [A3Error "path" "body" "This resource path does not exist."]
            around_ (withA3fake (Just status400) a3errMsg) $ do
                it "passes the error on if A3 signals failure" $ do

                    liftIO $ pendingWith "see BUG #321, too."

                    let resetReq = mkPwResetRequestJson "/principals/resets/dummypath" "newpass"
                    rsp <- request "POST" "password_reset" [ctJson] resetReq
                    shouldBeErr400WithCustomMessage (simpleStatus rsp) (simpleBody rsp)
                        "resource path does not exist"

        describe "an arbitrary request" $ with setupBackend $ do
            it "rejects bad session token mimicking A3" $ do
                let headers = [("X-User-Token", "invalid-token")]
                rsp <- request "POST" "dummy/endpoint" headers ""
                shouldBeErr400WithCustomMessage (simpleStatus rsp) (simpleBody rsp)
                    "\"Invalid user token\""

            it "throws an internal error if A3 is unreachable" $ do
                rsp <- request "POST" "dummy/endpoint" [ctJson] ""
                liftIO $ Status.statusCode (simpleStatus rsp) `shouldBe` 500
                -- Response body should be parseable as JSON
                let rspBody = simpleBody rsp
                liftIO $ (Aeson.decode rspBody :: Maybe Aeson.Value) `shouldSatisfy` isJust
                -- It should contain the quoted string "internal error"
                liftIO $ cs rspBody `shouldContain` ("\"internal error\"" :: String)


type TestState = (Application, ThentosConfig)

runTestState :: (TestState -> WaiSession a) -> TestState -> IO a
runTestState session (app, cfg) = runWaiSession (session (app, cfg)) app

withTestState :: Application -> ActionWith TestState -> IO ()
withTestState a3backend action = outsideTempDirectory $ \tmp ->
    setupBackend' [ YamlString . cs . unlines $
                      [ "log:"
                      , "  level: DEBUG"
                      , "  stdout: False"
                      , "  path: " ++ tmp </> "log" ]]
      >>= withApp a3backend . action

setupBackend :: IO Application
setupBackend = fst <$> setupBackend' []

setupBackend' :: [Source] -> IO (Application, ThentosConfig)
setupBackend' extraCfg = do
    as@(ActionState cfg _ connPool) <- thentosTestConfig' extraCfg >>= createActionState'
    mgr <- newManager defaultManagerSettings
    createGod connPool
    ((), ()) <- runActionWithPrivs [toCNF RoleAdmin] () as $
          autocreateMissingServices cfg
    let Just beConfig = Tagged <$> cfg >>. (Proxy :: Proxy '["backend"])
    return (serveApi mgr beConfig as, cfg)

ctJson :: Header
ctJson = ("Content-Type", "application/json")


-- * helper functions

-- | Compare the response status with an expected status code and check that the response
-- body is a JSON object that contains all of the specified custom strings.
shouldBeStatusXWithCustomMessages :: MonadIO m => (?loc :: CallStack) =>
    Int -> Status.Status -> LBS -> [String] -> m ()
shouldBeStatusXWithCustomMessages expectedCode rstStatus rspBody customMessages = do
    liftIO $ Status.statusCode rstStatus `shouldBe` expectedCode
    -- Response body should be parseable as JSON
    liftIO $ (Aeson.decode rspBody :: Maybe Aeson.Value) `shouldSatisfy` isJust
    liftIO $ for_ customMessages (cs rspBody `shouldContain`)

-- | Like 'shouldBeStatusXWithCustomMessage', with the expected code set tu 400.
-- We check the custom messages and the quoted string "error" are present in the JSON body.
shouldBeErr400WithCustomMessage :: MonadIO m => (?loc :: CallStack) =>
    Status.Status -> LBS -> String -> m ()
shouldBeErr400WithCustomMessage rstStatus rspBody customMessage =
    shouldBeStatusXWithCustomMessages 400 rstStatus rspBody ["\"error\"", customMessage]

-- | Create a JSON object describing an user.
-- Aeson.encode would strip the password, hence we do it by hand.
mkUserJson :: ST -> ST -> ST -> LBS
mkUserJson name email password = encodePretty $ object
  [ "data" .= object
      [ "adhocracy_core.sheets.principal.IUserBasic" .= object
          [ "name" ..= name
          ]
       , "adhocracy_core.sheets.principal.IUserExtended" .= object
          [ "email" ..= email
          ]
       , "adhocracy_core.sheets.principal.IPasswordAuthentication" .= object
          [ "password" ..= password
          ]
      ]
  , "content_type" ..= "adhocracy_core.resources.principal.IUser"
  ]

-- | Create a JSON object for a login request. Calling the ToJSON instance might be
-- considered cheating, so we do it by hand.
mkLoginRequest :: ST -> ST -> LBS
mkLoginRequest name pass = encodePretty $ object
    ["name" .= String name, "password" .= String pass]

-- | Create a JSON object for a PasswordResetRequest. Calling the ToJSON instance might be
-- considered cheating, so we do it by hand.
mkPwResetRequestJson :: ST -> ST -> LBS
mkPwResetRequestJson path pass = encodePretty $ object
    ["path" .= String path, "password" .= String pass]

-- | Start a faked A3 backend that always returns the same response, passed in as argument.
-- The status code defaults to 200.
withA3fake :: Maybe Status -> LBS -> IO () -> IO ()
withA3fake mStatus respBody = withApp $ staticReplyServer mStatus Nothing respBody

-- | Start an application prior to running the test, stopping it afterwards.
withApp :: Application -> IO () -> IO ()
withApp app test = bracket
    (startDaemon $ runSettings settings app)
    stopDaemon
    (const test)
  where
    settings = setHost "127.0.0.1" . setPort 8001 $ defaultSettings
