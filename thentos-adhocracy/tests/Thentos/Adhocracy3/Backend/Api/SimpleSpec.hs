{-# LANGUAGE ExistentialQuantification                #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE InstanceSigs                             #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TupleSections                            #-}
{-# LANGUAGE TypeSynonymInstances                     #-}
{-# LANGUAGE ViewPatterns                             #-}

module Thentos.Adhocracy3.Backend.Api.SimpleSpec
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value(String), object, (.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Functor ((<$>))
import Data.Maybe (isJust)
import Data.String.Conversions (LBS, ST, cs)
import Network.Wai.Test (srequest, simpleBody, simpleStatus)
import Test.Hspec (Spec, after, before, describe, hspec, it, shouldBe, shouldSatisfy)
import Test.QuickCheck (property)

import qualified Data.Aeson as Aeson
import qualified Data.Text as ST
import qualified Network.HTTP.Types.Status as Status

import Thentos.Adhocracy3.Backend.Api.Simple
import Thentos.Adhocracy3.Types

import Thentos.Test.Arbitrary ()
import Thentos.Test.Types
import Thentos.Test.Core hiding (setupTestBackend)
import qualified Thentos.Test.Core (setupTestBackend)

setupTestBackend :: IO (BTS DB)
setupTestBackend = Thentos.Test.Core.setupTestBackend serveApi


tests :: IO ()
tests = hspec spec

spec :: Spec
spec =
    describe "Thentos.Backend.Api.Adhocracy3" $ do
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
                     `shouldBe` Left "Not a valid email address: anna@"

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

        describe "login" . before setupTestBackend . after teardownTestBackend $
            it "rejects bad credentials mimicking A3" $
                \bts -> runTestBackend bts $ do

                    let reqBody = encodePretty . object $
                            [ "name"     .= String "Anna Müller"
                            , "password" .= String "this-is-wrong"
                            ]

                    rsp <- srequest $ makeSRequest "POST" "login_username" [] reqBody
                    shouldBeErr400WithCustomMessage (simpleStatus rsp) (simpleBody rsp)
                        "\"User doesn't exist or password is wrong\""

        describe "arbitrary requests" . before setupTestBackend . after teardownTestBackend $
            it "rejects bad session token mimicking A3" $
                \bts -> runTestBackend bts $ do

                    let headers = [("X-User-Token", "invalid-token")]
                    rsp <- srequest $ makeSRequest "POST" "dummy/endpoint" headers ""
                    shouldBeErr400WithCustomMessage (simpleStatus rsp) (simpleBody rsp)
                        "\"Invalid user token\""

-- Disabled because account activation now requires the A3 backend to run
--        describe "activate_account" . before setupTestBackend . after teardownTestBackend $
--            it "rejects bad path mimicking A3" $
--                \bts -> runTestBackend bts $ do
--
--                    let reqBody = encodePretty . object $
--                            [ "path" .= String "/activate/no-such-path" ]
--
--                    rsp <- srequest $ makeSRequest "POST" "activate_account" [] reqBody
--                    shouldBeErr400WithCustomMessage (simpleStatus rsp) (simpleBody rsp)
--                        "\"Unknown or expired activation path\""
  where
    shouldBeErr400WithCustomMessage :: MonadIO m => Status.Status -> LBS -> ST -> m ()
    shouldBeErr400WithCustomMessage rstStatus rspBody customMessage = do
        liftIO $ Status.statusCode rstStatus `shouldBe` 400
        -- Response body should be parseable as JSON
        liftIO $ (Aeson.decode rspBody :: Maybe Aeson.Value) `shouldSatisfy` isJust
        let rspText = cs rspBody :: ST
        -- It should contain the quoted string "error" as well as the customMessage
        liftIO $ rspText `shouldSatisfy` ST.isInfixOf "\"error\""
        liftIO $ rspText `shouldSatisfy` ST.isInfixOf customMessage

-- FIXME Disabled since user creation now requires a running A3 backend and we don't have
-- that in the tests.
--
-- Additional imports required for disabled test:
-- import Control.Lens ((^.))
-- import Control.Monad.IO.Class (liftIO)
-- import Data.Acid.Advanced (query')
-- import Data.String.Conversions (cs, (<>))
-- import Network.Wai.Test (srequest, simpleStatus, simpleBody)
-- import Test.Hspec (shouldSatisfy)
-- import System.Exit (ExitCode(ExitSuccess))
-- import System.FilePath ((</>))
-- import System.Process (readProcess, readProcessWithExitCode)
-- import qualified Data.Map as Map
-- import qualified Data.HashMap.Strict as HashMap
-- import qualified Network.HTTP.Types.Status as C
-- import Thentos.Action.Core
-- import qualified Thentos.Transaction as T
-- import Test.Types
--
--      describe "create user" . before setupTestBackend . after teardownTestBackend $
--          it "works" $
--              \ bts@(BTS tcfg ast@(ActionState (st, _, _)) _ _ _) -> runTestBackend bts $ do
--
--                  let rq1 = mkUserJson "Anna Müller" "anna@example.org" "EckVocUbs3"
--
--                  -- Appending trailing newline since servant-server < 0.4.1 couldn't handle it
--                  rsp1 <- srequest $ makeSRequest "POST" "/principals/users" [] $ rq1 <> "\n"
--                  liftIO $ C.statusCode (simpleStatus rsp1) `shouldBe` 201
--
--                  -- Extract user path from response
--                  let respJson = Aeson.decode (simpleBody rsp1) :: Maybe Aeson.Value
--                      mUserPath = case respJson of
--                          Just (Aeson.Object m) -> HashMap.lookup "path" m
--                          _                     -> error "Response is not a JSON object"
--
--                  -- Check that it looks as it should
--                  case mUserPath of
--                      Just pathItem -> case pathItem of
--                          Aeson.String userPath -> do
--                              userId <- liftIO . runAction ast . userIdFromPath . Path $ userPath
--                              liftIO $ fromUserId userId `shouldSatisfy` (> 0)
--                          _ -> error "'path' in response is not a string"
--                      Nothing -> error "Response doesn't contain 'path' field"
--
--                  Right (db :: DB) <- query' st T.SnapShot
--                  let [(ConfirmationToken confTok, _)] = Map.toList $ db ^. dbUnconfirmedUsers
--
--                  -- Check that activation email was sent and contains the confirmation token.
--                  -- We use system calls to "grep" here which is ugly but works, while reading the
--                  -- log file within Haskell doesn't (openFile: resource busy (file is locked)).
--                  let logfile = tcfg ^. tcfgTmp </> "everything.log"
--                  (statusCode, _, _) <- liftIO $ readProcessWithExitCode "grep"
--                      ["-q", "Subject: Thentos account creation", logfile] ""
--                  liftIO $ statusCode `shouldBe` ExitSuccess
--                  loggedLine <- liftIO $ readProcess "grep" ["\"Please go to ", logfile] ""
--                  -- The grepped line should contain the confirmation token
--                  liftIO $ confTok `shouldSatisfy` (`ST.isInfixOf` cs loggedLine)
--
--                  let rq2 = Aeson.encode . ActivationRequest . Path $ "/activate/" <> confTok
--                  rsp2 <- srequest $ makeSRequest "POST" "/activate_account" [] rq2
--                  liftIO $ C.statusCode (simpleStatus rsp2) `shouldBe` 201
--
--                  let sessTok = case Aeson.eitherDecode $ simpleBody rsp2 of
--                        Right (RequestSuccess _ t) -> t
--                        bad -> error $ show bad
--
--                  liftIO $ sessTok `shouldSatisfy` (not . ST.null . fromThentosSessionToken)
--                  return ()

        -- FIXME currently not working because of Servant quirks on failures
        --describe "create user errors" . before setupTestBackend
        --                              . after teardownTestBackend $
        --    it "rejects users with short passwords" $
        --        \ bts@(BTS _ (ActionState (_, _, _)) _ _ _) -> runTestBackend bts $ do
        --
        --            let rq1 = mkUserJson "Anna Müller" "anna@example.org" "short"
        --            rsp1 <- srequest $ makeSRequest "POST" "/principals/users" [] $ rq1
        --            liftIO $ C.statusCode (simpleStatus rsp1) `shouldBe` 400
        --            liftIO $ simpleBody rsp1 `shouldBe` "..."

        -- FIXME further stuff that can only be tested after Servant has been fixed:
        -- (1) An error is returned if the specified user name or email doesn't exist
        -- (2) An error is returned if the wrong password is specified


-- | Create a JSON object describing an user.
-- Aeson.encode would strip the password, hence we do it by hand.
mkUserJson :: ST -> ST -> ST -> LBS
mkUserJson name email password = encodePretty . object $
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
