{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Thentos.Frontend.StateSpec where

import Control.Lens ((^.), (%~))
import Control.Monad (when)
import Control.Monad.State (modify, gets, liftIO)
import Data.Maybe (catMaybes, fromMaybe)
import Data.String.Conversions (ST, LBS, SBS, cs)
import Network.HTTP.Types (RequestHeaders, methodGet, methodPost)
import Network.Wai (Application)
import Network.Wai.Test (SResponse, simpleBody, simpleHeaders, simpleStatus)
import Servant (Proxy(Proxy), ServerT, Capture, QueryParam, Post, Get, JSON, (:<|>)((:<|>)), (:>))
import Test.Hspec (Spec, hspec, before, around, describe, it,
                   shouldBe, shouldContain, shouldSatisfy, shouldNotSatisfy, pendingWith)
import Test.Hspec.Wai (request)
import Servant.Server.Internal.ServantErr (errHTTPCode, errHeaders, errBody)

import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Network.HTTP.Types.Status as HT
import qualified Network.Wreq as Wreq

import Thentos.Action.Core
import Thentos.Config
import Thentos.Frontend.Handlers.Combinators (redirect')
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

    describe "the FAction monad, via hspec-wai" . before testApp $ do
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

        it "keeps state even when exceptions are thrown" $ do
            presp <- request methodPost "heya?crash=True" [] ""
            gresp <- request methodGet "" (getCookie presp) ""
            liftIO $ simpleBody gresp `shouldBe`
                (cs . show . fmap show $ [FrontendMsgSuccess "heya"])

        it "renders 404 correctly." $ do
            resp <- request methodGet "/wef/yo" [] ""
            liftIO $ HT.statusCode (simpleStatus resp) `shouldBe` 404
            liftIO $ pendingWith "rendering of implicit servant 404 (thrown by `HasServer (:>)`) cannot be customized."
            -- FIXME: it would be easy to get this to work with a catch-all combinator
            -- (https://github.com/haskell-servant/servant/issues/257), or in a small middleware.
            liftIO $ cs (simpleBody resp) `shouldContain` ("<!DOCTYPE HTML>" :: String)

    describe "the FAction monad, via warp" . around (withFrontendAndBackend "thentos_test_db") $ do
        let mkurl :: ActionState -> ST -> String
            mkurl (ActionState (_, _, cfg)) = cs . (exposeUrl (getFrontendConfig cfg) <//>)

            post :: ActionState -> Maybe ST -> Maybe ST -> IO (Wreq.Response LBS)
            post astate name pass = liftIO . Wreq.post (mkurl astate "/user/login") . catMaybes $
                [("/user/login.name" Wreq.:=) <$> name, ("/user/login.password" Wreq.:=) <$> pass]

            hasDfError :: SBS -> LBS -> Bool
            hasDfError fieldName responseBody = case AP.parse p (cs responseBody) of
                  AP.Done _ _ -> True
                  _        -> False
              where
                p = AP.manyTill AP.anyChar $ do
                      _ <- AP.string fieldName
                      _ <- AP.many' $ AP.satisfy AP.isSpace
                      _ <- AP.string "must not be empty"
                      return ()

        it "rejects bad login requests" $ \astate -> do
            -- (just checking that parser works)
            "ow!\n  must not be empty\n" `shouldSatisfy` hasDfError "ow!"
            "........" `shouldNotSatisfy` hasDfError "ow!"

            resp <- post astate Nothing Nothing
            (resp ^. Wreq.responseBody) `shouldSatisfy` hasDfError "name"
            (resp ^. Wreq.responseBody) `shouldSatisfy` hasDfError "password"

            resp2 <- post astate (Just "god") Nothing
            (resp2 ^. Wreq.responseBody) `shouldNotSatisfy` hasDfError "name"
            (resp2 ^. Wreq.responseBody) `shouldSatisfy` hasDfError "password"

            resp3 <- post astate Nothing (Just "god")
            (resp3 ^. Wreq.responseBody) `shouldSatisfy` hasDfError "name"
            (resp3 ^. Wreq.responseBody) `shouldNotSatisfy` hasDfError "password"

        it ("rejects login requests of non-existent users or " ++
            "with wrong credentials (indistinguishably)") $ \astate -> do
            resp <- post astate (Just "bad") (Just "user")
            cs (resp ^. Wreq.responseBody) `shouldContain` ("FrontendMsgError" :: String)

            pendingWith $
                "part of this test not implemented: wrong credentials, " ++
                "indistinguishability of the two cases"

        it "accepts login requests of existing users" $ \astate -> do
            resp <- post astate (Just "god") (Just "god")
            cs (resp ^. Wreq.responseBody) `shouldContain` ("FrontendMsgSuccess" :: String)


getCookie :: SResponse -> RequestHeaders
getCookie = fmap f . simpleHeaders
  where
    f ("Set-Cookie", c) = ("Cookie", c)
    f hdr = hdr


type TestApi =
       Capture "id" ST :> QueryParam "crash" Bool :> Post '[JSON] ()
  :<|> Get '[JSON] [ST]

testApi :: ServerT TestApi FAction
testApi = _post :<|> _read
  where
    _post msg (fromMaybe False -> _crash) = do
        modify $ fsdMessages %~ (FrontendMsgSuccess msg :)
        when _crash $ redirect' "/wef"
    _read = gets ((cs . show <$>) . (^. fsdMessages))

testApp :: IO Application
testApp = createActionState "thentos_test" thentosTestConfig
      >>= serveFAction (Proxy :: Proxy TestApi) testApi
