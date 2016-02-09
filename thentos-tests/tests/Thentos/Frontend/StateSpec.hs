{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Thentos.Frontend.StateSpec where

import Control.Exception (finally)
import Data.Pool (destroyAllResources)
import Network.HTTP.Types (Header, RequestHeaders, methodGet, methodPost)
import Network.Wai (Application)
import Network.Wai.Test (SResponse, simpleBody, simpleHeaders, simpleStatus)
import Servant (ServerT, Capture, QueryParam, Post, Get, JSON, (:<|>)((:<|>)), (:>))
import Test.Hspec (Spec, hspec, before, around, describe, it,
                   shouldBe, shouldContain, shouldSatisfy, shouldNotSatisfy, pendingWith)
import Test.Hspec.Wai (request) --, shouldRespondWith)
import Text.Blaze.Html (Html)
import Text.Digestive.Blaze.Html5 (inputText, label, inputSubmit)
import Text.Digestive.Form (Form, text, (.:))
import Text.Digestive.View (View)
import Servant.Server.Internal.ServantErr (errHTTPCode, errHeaders, errBody)

import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Network.HTTP.Types.Status as HT
import qualified Network.Wreq as Wreq
import qualified Test.Hspec.Wai as HW
import qualified Text.Xml.Lens as HL

import Thentos (createDefaultUser)
import Thentos.Action.Types
import Thentos.Config
import Thentos.Frontend.Handlers.Combinators
import Thentos.Frontend.Pages.Core
import Thentos.Frontend.State
import Thentos.Frontend.Types
import Thentos.Prelude
import Thentos.Test.Core
import Thentos.Types

import Debug.Trace

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

    describe "the FAction monad, on a test API, via hspec-wai" . before testApp $ do
        it "is initialized with empty message queue" $ do
            resp <- HW.get ""
            liftIO $ simpleBody resp `shouldBe` "[]"

        it "posts accumulate into message queue" $ do
            presp <- HW.post "heya" ""
            gresp <- request methodGet "" (getCookie presp) ""
            liftIO $ simpleBody gresp `shouldBe`
                (cs . show . fmap show $ [FrontendMsgSuccess "heya"])

            presp2 <- request methodPost "ping" (getCookie gresp) ""
            gresp2 <- request methodGet "" (getCookie presp2) ""
            liftIO $ simpleBody gresp2 `shouldBe`
                (cs . show . fmap show $ [FrontendMsgSuccess "ping", FrontendMsgSuccess "heya"])

        it "keeps state even when exceptions are thrown" $ do
            presp <- HW.post "heya?crash=True" ""
            gresp <- request methodGet "" (getCookie presp) ""
            liftIO $ simpleBody gresp `shouldBe`
                (cs . show . fmap show $ [FrontendMsgSuccess "heya"])

        it "renders 404 correctly." $ do
            resp <- HW.get "/wef/yo"
            liftIO $ HT.statusCode (simpleStatus resp) `shouldBe` 404
            liftIO $ pendingWith "rendering of implicit servant 404 (thrown by `HasServer (:>)`) cannot be customized."
            -- FIXME: it would be easy to get this to work with a catch-all combinator
            -- (https://github.com/haskell-servant/servant/issues/257), or in a small middleware.
            liftIO $ cs (simpleBody resp) `shouldContain` ("<!DOCTYPE HTML>" :: String)

    describe "the FAction monad, via warp" . around withFrontendAndBackend $ do
        let mkurl :: ActionState -> ST -> String
            mkurl as = cs . (exposeUrl (getFrontendConfig (as ^. aStConfig)) <//>)

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

    describe "the CSRF protection, on a test API, via hspec-wai" . around withTestApp $ do
        it "returns a valid CSRF token" $ \(hdr :: Header) -> do
            presp <- HW.post "heya" ""
            gresp <- request methodGet "csrf" (getCookie presp) ""
            let part = simpleBody gresp ^. -- HL.html . HL.node "form" . HL.text
                            HL.html . HL.node "body" . HL.node "form" .
                            HL.node "input" . HL.attributed (ix "type".only "submit") .
                            HL.attr "value" . _Just
            trace (show hdr) (return ())
            trace (show (getCookie presp)) (return ())
            trace (show (simpleBody gresp)) (return ())
            trace (show part) (return ())
            liftIO $ cs part `shouldContain` ("_csrf=" :: String)

getCookie :: SResponse -> RequestHeaders
getCookie = fmap f . simpleHeaders
  where
    f ("Set-Cookie", c) = ("Cookie", c)
    f hdr = hdr


type TestApi =
       Capture "id" ST :> QueryParam "crash" Bool :> Post '[JSON] ()
  :<|> Get '[JSON] [ST]
  :<|> "csrf" :> FormH [FrontendMsg]

csrfApi :: ServerT (FormH [FrontendMsg]) FAction
csrfApi = formH "/csrf" p1 p2 r
  where
    p1 :: Form Html FAction [FrontendMsg]
    p1 = (pure . FrontendMsgSuccess) <$> "message" .: text Nothing

    csrfPage :: FrontendSessionData -> View Html -> ST -> Html
    csrfPage fsd v formAction = basePagelet fsd "Add message" $ do
        csrfProofForm fsd v formAction $ do
            label "message" v "Message:"
            inputText "message" v
            inputSubmit "Post message"

    p2 :: [FrontendMsg] -> FAction Html
    p2 messages = do
        fsdMessages %= (messages <>)
        fsd <- get
        pure $ basePagelet fsd "Messages" (pure ())

    r :: View Html -> ST -> FAction Html
    r = showPageWithMessages csrfPage

testApi :: ServerT TestApi FAction
testApi = post_ :<|> read_ :<|> csrfApi
  where
    post_ msg (fromMaybe False -> crash_) = do
        fsdMessages %= (FrontendMsgSuccess msg :)
        when crash_ $ redirect' "/wef"
    read_ = uses fsdMessages (cs . show <$>)

testApp :: IO Application
testApp = createActionState >>= serveFAction (Proxy :: Proxy TestApi) testApi

{-
testAppWithLogin :: IO ([String], Application)
testAppWithLogin = do
    as <- createActionState
    (thentosSession, hdr) <- loginAsDefaultUser as
    app <- serveFAction (Proxy :: Proxy TestApi) testApi
    return (hdr, app)
-}

--withTestApp :: MonadIO m => (Header -> m r) -> m r
withTestApp :: (Header -> IO Application) -> IO Application -- HW.WaiSession r) -> HW.WaiSession r
withTestApp test = do
    st@(ActionState _ _ connPool) <- liftIO createActionState
    _app <- liftIO $ serveFAction (Proxy :: Proxy TestApi) testApi st
    liftIO (createDefaultUser st)
    (_thentosSession, hdr) <- liftIO $ loginAsDefaultUser st
    test hdr
