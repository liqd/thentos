{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}

module Thentos.Action.SimpleAuthSpec where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import Data.Configifier (Source(YamlString), Tagged(Tagged), (>>.))
import LIO (LIO)
import Network.Wai (Application)
import Servant.API ((:>), Get, JSON)
import Servant.Server (serve, enter)
import Test.Hspec (Spec, describe, context, it, around, hspec, shouldBe)

import qualified Data.Aeson as Aeson
import qualified Network.Wreq as Wreq

import Thentos.Prelude
import Thentos.Action.Core
import Thentos.Action.Types
import Thentos.Action.SimpleAuth
import Thentos.Action.Unsafe
import Thentos.Backend.Api.Auth.Types
import Thentos.Backend.Core
import Thentos.Config
import Thentos.Types

import Thentos.Test.Arbitrary ()
import Thentos.Test.Config
import Thentos.Test.Core


tests :: IO ()
tests = hspec spec

spec :: Spec
spec = do
    specWithActionState
    specWithBackends

type Act = LIO DCLabel -- ActionStack (ActionError Void) ()

runActE :: Act a -> IO (Either (ActionError Void) a)
runActE = runLIOE
runAct :: Act a -> IO a
runAct = ioExc' . runActE

setTwoGroups :: MonadThentosIO m => m ()
setTwoGroups = extendClearanceOnPrincipals [GroupAdmin, GroupUser]

setClearanceUid :: MonadThentosIO m => Integer -> m ()
setClearanceUid uid = extendClearanceOnPrincipals [UserA $ UserId uid]
                   >> extendClearanceOnPrincipals [GroupUser]

setClearanceSid :: MonadThentosIO m => Integer -> m ()
setClearanceSid sid = extendClearanceOnPrincipals [ServiceA . ServiceId . cs . show $ sid]

specWithActionState :: Spec
specWithActionState = do
    describe "assertAuth" $ do
        it "throws an error on False" $ do
            Left (ActionErrorAnyLabel _) <- runActE (assertAuth $ pure False :: Act ())
            return ()
        it "returns () on True" $ do
            runAct (assertAuth $ pure True :: Act ())

    describe "hasUserId" $ do
        it "returns True on if uid matches" $ do
            True <- runAct (setClearanceUid 3 >> hasUserId (UserId 3) :: Act Bool)
            return ()
        it "returns False on if uid does not match" $ do
            False <- runAct (hasUserId (UserId 3) :: Act Bool)
            False <- runAct (setClearanceUid 5 >> hasUserId (UserId 3) :: Act Bool)
            return ()

    describe "hasServiceId" $ do
        it "returns True on if sid matches" $ do
            True <- runAct (setClearanceSid 3 >> hasServiceId (ServiceId "3") :: Act Bool)
            return ()
        it "returns False on if sid does not match" $ do
            False <- runAct (hasServiceId (ServiceId "3") :: Act Bool)
            False <- runAct (setClearanceSid 5 >> hasServiceId (ServiceId "3") :: Act Bool)
            return ()
        it "can distinguish uid and sid" $ do
            False <- runAct (setClearanceUid 3 >> hasServiceId (ServiceId "3") :: Act Bool)
            return ()

    describe "hasGroup" $ do
        it "returns True if group is present" $ do
            True <- runAct (setClearanceUid 3 >> hasGroup GroupUser :: Act Bool)
            True <- runAct (setClearanceUid 5 >> hasGroup GroupUser :: Act Bool)
            True <- runAct (setTwoGroups >> hasGroup GroupUser :: Act Bool)
            return ()
        it "returns False if group is missing" $ do
            False <- runAct (hasGroup GroupUser :: Act Bool)
            False <- runAct (setTwoGroups >> hasGroup GroupServiceAdmin :: Act Bool)
            return ()


withPrivIpBackend :: [String] -> (HttpConfig -> IO r) -> IO r
withPrivIpBackend allowIps testCase = do
    cfg <- thentosTestConfig' [YamlString . ("allow_ips: " <>) . cs . show $ allowIps]
    as <- createActionState' cfg

    let Just becfg = Tagged <$> (as ^. aStConfig) >>. (Proxy :: Proxy '["backend"])
    bracket (forkIO $ runWarpWithCfg becfg $ serveApi as)
        killThread
        (\_ -> testCase becfg)

type Api = ThentosAuth :> Get '[JSON] Bool

serveApi :: ActionState -> Application
serveApi as = serve (Proxy :: Proxy Api) $
    (\creds -> enter (enterAction () as baseActionErrorToServantErr creds) hasPrivilegedIP)


specWithBackends :: Spec
specWithBackends = describe "hasPrivilegedIp" $ do
    let works :: [String] -> Bool -> Spec
        works ips y = context ("with allow_ips = " ++ show ips) . around (withPrivIpBackend ips) $
            it ("returns " ++ show y ++ " for requests from localhost") $ \httpCfg -> do
                resp <- Wreq.get (cs $ exposeUrl httpCfg)
                resp ^. Wreq.responseBody `shouldBe` Aeson.encode y

    works ["127.0.0.1"] True
    works ["1.2.3.4"] False
    works [] False
    works ["::1"] False
    works ["fe80::42:d4ff:fec0:544d"] False
