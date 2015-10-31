{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}

{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Thentos.Frontend.StateSpec where

import Control.Lens ((^.), (%~))
import Control.Monad.State (modify, gets, liftIO)
import Data.String.Conversions (ST, LBS, cs)
import Network.HTTP.Types (RequestHeaders, methodGet, methodPost)
import Network.Wai (Application)
import Network.Wai.Test (SResponse, simpleBody, simpleHeaders, simpleStatus)
import Servant (Proxy(Proxy), ServerT, Capture, Post, Get, JSON, (:<|>)((:<|>)), (:>))
import Test.Hspec (Spec, hspec, before, describe, it, shouldBe, shouldContain, pendingWith)
import Test.Hspec.Wai (request)
import Servant.Server.Internal.ServantErr (errHTTPCode, errHeaders, errBody)

import qualified Network.HTTP.Types.Status as C

import Thentos.Action.Core
import Thentos.Frontend.State
import Thentos.Frontend.Types
import Thentos.Types

import Thentos.Test.Config
import Thentos.Test.Core


tests :: IO ()
tests = hspec spec

spec :: Spec
spec = describe "Thentos.Frontend.State" $ do
    spec_frontendState

spec_frontendState :: Spec
spec_frontendState = do
    describe "fActionServantErr" $ do
        it "redirects with correct status and location header." $ do
            e <- fActionServantErr (ActionErrorThentos . OtherError . FActionError303 $ "/there")
            errHTTPCode e `shouldBe` 303
            errHeaders e `shouldContain` [("Location", "/there")]

        it "renders 404 correctly." $ do
            e <- fActionServantErr . ActionErrorThentos . OtherError $ FActionError404
            errHTTPCode e `shouldBe` 404
            cs (errBody e) `shouldContain` ("<!DOCTYPE HTML>" :: String)

    describe "the FAction monad" . before testApp $ do
        it "is initialized with empty message queue" $ do
            resp <- request methodGet "" [] ""
            liftIO $ simpleBody resp `shouldBe` "[]"

        it "posts accumulate into message queue" $ do
            presp <- request methodPost "heya" [] ""
            gresp <- request methodGet "" (getCookie presp) ""
            liftIO $ simpleBody gresp `shouldBe`
                (cs . show . fmap show $ [FrontendMsgSuccess "heya"])

            presp2 <- request methodPost "ping" (getCookie gresp) ""
            gresp2 <- request methodGet "" (getCookie presp2) ""
            liftIO $ simpleBody gresp2 `shouldBe`
                (cs . show . fmap show $ [FrontendMsgSuccess "ping", FrontendMsgSuccess "heya"])

        it "renders 404 correctly." $ do
            resp <- request methodGet "/wef/yo" [] ""
            liftIO $ C.statusCode (simpleStatus resp) `shouldBe` 404
            liftIO $ pendingWith "rendering of implicit servant 404 (thrown by `HasServer (:>)`) defunct."

            -- FIXME: the code in '' is in place.  it would be easy to get this to work with a
            -- catch-all combinator (https://github.com/haskell-servant/servant/issues/257).
            liftIO $ cs (simpleBody resp) `shouldContain` ("<!DOCTYPE HTML>" :: String)


getCookie :: SResponse -> RequestHeaders
getCookie = fmap f . simpleHeaders
  where
    f ("Set-Cookie", c) = ("Cookie", c)
    f hdr = hdr


type TestApi =
       Capture "id" ST :> Post '[JSON] ()
  :<|> Get '[JSON] [ST]

testApi :: ServerT TestApi FAction
testApi = _post :<|> _read
  where
    _post msg = modify $ fsdMessages %~ (FrontendMsgSuccess msg :)
    _read = gets ((cs . show <$>) . (^. fsdMessages))

testApp :: IO Application
testApp = createActionState "thentos_test" thentosTestConfig
      >>= serveFAction (Proxy :: Proxy TestApi) testApi
