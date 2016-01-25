{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}

module Thentos.Action.SimpleAuthSpec where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import Control.Lens ((^.))
import Data.Configifier (Source(YamlString), Tagged(Tagged), (>>.))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (cs, (<>))
import Data.Void (Void)
import Network.Wai (Application)
import Servant.API ((:>), Get, JSON)
import Servant.Server (serve, enter)
import Test.Hspec (Spec, describe, context, it, before, around, hspec, shouldBe)

import qualified Data.Aeson as Aeson
import qualified Network.Wreq as Wreq

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

type Act = Action (ActionError Void) ()

setTwoRoles :: Action e s ()
setTwoRoles = extendClearanceOnPrincipals [RoleAdmin, RoleUser]

setClearanceUid :: Integer -> Action e s ()
setClearanceUid uid = extendClearanceOnPrincipals [UserA $ UserId uid]
                   >> extendClearanceOnPrincipals [RoleUser]

setClearanceSid :: Integer -> Action e s ()
setClearanceSid sid = extendClearanceOnPrincipals [ServiceA . ServiceId . cs . show $ sid]


mkActionState :: IO ActionState
mkActionState = do
    actionState <- createActionState
    createGod (actionState ^. aStDb)
    return actionState

specWithActionState :: Spec
specWithActionState = before mkActionState $ do
    describe "assertAuth" $ do
        it "throws an error on False" $ \sta -> do
            (Left (ActionErrorAnyLabel _), ())
                <- runActionE () sta (assertAuth $ pure False :: Act ())
            return ()
        it "returns () on True" $ \sta -> do
            fst <$> runAction () sta (assertAuth $ pure True :: Act ())

    describe "hasUserId" $ do
        it "returns True on if uid matches" $ \sta -> do
            (True, ()) <- runAction () sta (setClearanceUid 3 >> hasUserId (UserId 3) :: Act Bool)
            return ()
        it "returns False on if uid does not match" $ \sta -> do
            (False, ()) <- runAction () sta (hasUserId (UserId 3) :: Act Bool)
            (False, ()) <- runAction () sta (setClearanceUid 5 >> hasUserId (UserId 3) :: Act Bool)
            return ()

    describe "hasServiceId" $ do
        it "returns True on if sid matches" $ \sta -> do
            (True, ()) <- runAction () sta
                (setClearanceSid 3 >> hasServiceId (ServiceId "3") :: Act Bool)
            return ()
        it "returns False on if sid does not match" $ \sta -> do
            (False, ()) <- runAction () sta
                (hasServiceId (ServiceId "3") :: Act Bool)
            (False, ()) <- runAction () sta
                (setClearanceSid 5 >> hasServiceId (ServiceId "3") :: Act Bool)
            return ()
        it "can distinguish uid and sid" $ \sta -> do
            (False, ()) <- runAction () sta
                (setClearanceUid 3 >> hasServiceId (ServiceId "3") :: Act Bool)
            return ()

    describe "hasRole" $ do
        it "returns True if role is present" $ \sta -> do
            (True, ()) <- runAction () sta (setClearanceUid 3 >> hasRole RoleUser :: Act Bool)
            (True, ()) <- runAction () sta (setClearanceUid 5 >> hasRole RoleUser :: Act Bool)
            (True, ()) <- runAction () sta (setTwoRoles >> hasRole RoleUser :: Act Bool)
            return ()
        it "returns False if role is missing" $ \sta -> do
            (False, ()) <- runAction () sta (hasRole RoleUser :: Act Bool)
            (False, ()) <- runAction () sta (setTwoRoles >> hasRole RoleServiceAdmin :: Act Bool)
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
