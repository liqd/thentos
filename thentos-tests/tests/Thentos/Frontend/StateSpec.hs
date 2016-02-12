{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Thentos.Frontend.StateSpec where

import Control.Exception (finally)
import Data.Pool (destroyAllResources)
import Network.HTTP.Types (RequestHeaders, methodGet, methodPost)
import Network.Wai (Application)
import Network.Wai.Test (SResponse, simpleBody, simpleHeaders, simpleStatus)
import Servant (ServerT, Capture, QueryParam, Post, Get, JSON, (:<|>)((:<|>)), (:>))
import Test.Hspec (Spec, hspec, before, around, describe, it, shouldBe, shouldNotBe, shouldContain,
                   shouldSatisfy, shouldNotSatisfy, pendingWith)
import Test.Hspec.Wai (request, shouldRespondWith)
import Test.Hspec.Wai.Internal (WaiSession(..), runWaiSession)
import Text.Blaze.Html (Html)
import Text.Digestive.Blaze.Html5 (inputText, label, inputSubmit)
import Text.Digestive.Form (Form, text, (.:))
import Text.Digestive.View (View)
import Servant.Server.Internal.ServantErr (errHTTPCode, errHeaders, errBody)

import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.Text as ST
import qualified Network.HTTP.Types.Status as HT
import qualified Network.Wreq as Wreq
import qualified Test.Hspec.Wai as HW
import qualified Text.Xml.Lens as HL

import Thentos (createDefaultUser)
import Thentos.Action.Types
import Thentos.Config hiding (getDefaultUser)
import Thentos.Frontend.Handlers.Combinators
import Thentos.Frontend.Pages.Core
import Thentos.Frontend.State
import Thentos.Frontend.CSRF
import Thentos.Frontend.Types
import Thentos.Prelude
import Thentos.Test.Core
import Thentos.Test.Config (getDefaultUser)
import Thentos.Types

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

    describe "the CSRF protection, on a test API, via hspec-wai" $ do
        let token r = CsrfToken $ simpleBody r ^.
                        HL.html . HL.node "body" . HL.node "form" .
                        HL.node "input" . HL.attributed (ix "type".only "hidden") .
                        HL.attr "value" . _Just
            newSession     = getCookie <$> request methodPost "session" [] ""
            getCsrf hdrs   = token <$> request methodGet "csrf" hdrs ""
            tamperChar 'a' = '0'
            tamperChar '0' = 'a'
            tamperChar  c  = c
            tamper (CsrfToken t) = CsrfToken (ST.map tamperChar t)
            postCsrf c (CsrfToken t) m =
                request methodPost "csrf"
                  (("Content-Type", "application/x-www-form-urlencoded"):c)
                  (cs $ "/csrf._csrf=" <> t <> "&/csrf.message=" <> m)

        it "does not return any CSRF token without a valid session" . withTestApp $ do
            _ <- newSession
            t <- getCsrf []
            liftIO $ t `shouldNotSatisfy` validFormatCsrfToken

        it "should reject a post request with a valid CSRF token" . withTestApp $ do
            c <- newSession
            t <- getCsrf []
            postCsrf c t "Hey" `shouldRespondWith` 500

        it "returns a well-formed CSRF token" . withTestApp $ do
            c <- newSession
            t <- getCsrf c
            liftIO $ t `shouldSatisfy` validFormatCsrfToken

        it "should accept a valid CSRF token" . withTestApp $ do
            c <- newSession
            t <- getCsrf c
            postCsrf c t "Hey" `shouldRespondWith` 201

        it "should reject a post request with a tampered CSRF token" . withTestApp $ do
            c <- newSession
            t <- getCsrf c
            postCsrf c (tamper t) "Hey" `shouldRespondWith` 500

        it "returns different CSRF tokens on different queries" . withTestApp $ do
            c <- newSession
            t0 <- getCsrf c
            t1 <- getCsrf c
            liftIO $ t0 `shouldSatisfy` validFormatCsrfToken
            liftIO $ t1 `shouldSatisfy` validFormatCsrfToken
            liftIO $ t0 `shouldNotBe` t1

        it "should accept two requests in-order" . withTestApp $ do
            c <- newSession
            t0 <- getCsrf c
            t1 <- getCsrf c
            postCsrf c t0 "Hey" `shouldRespondWith` 201
            postCsrf c t1 "Bye" `shouldRespondWith` 201

        it "should accept two requests in reverse order" . withTestApp $ do
            c <- newSession
            t0 <- getCsrf c
            t1 <- getCsrf c
            postCsrf c t1 "Bye" `shouldRespondWith` 201
            postCsrf c t0 "Hey" `shouldRespondWith` 201

getCookie :: SResponse -> RequestHeaders
getCookie = fmap f . simpleHeaders
  where
    f ("Set-Cookie", c) = ("Cookie", c)
    f hdr = hdr


type TestApi =
       "session" :> Post '[JSON] ()
  :<|> "csrf" :> FormH [FrontendMsg]
  :<|> Capture "id" ST :> QueryParam "crash" Bool :> Post '[JSON] ()
  :<|> Get '[JSON] [ST]

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

testApi :: Maybe FrontendSessionLoginData -> ServerT TestApi FAction
testApi mfsl = session :<|> csrfApi :<|> post_ :<|> read_
  where
    session = fsdLogin .= mfsl
    post_ msg (fromMaybe False -> crash_) = do
        fsdMessages %= (FrontendMsgSuccess msg :)
        when crash_ $ redirect' "/wef"
    read_ = uses fsdMessages (cs . show <$>)

testApp :: IO Application
testApp = createActionState >>= serveFAction (Proxy :: Proxy TestApi) (testApi Nothing)

withTestApp :: WaiSession () -> IO ()
withTestApp test = do
    st@(ActionState cfg _ connPool) <- createActionState
    (do createDefaultUser st
        (tok, _) <- loginAsDefaultUser st
        (uid, _, _) <- getDefaultUser cfg connPool
        let mfsl = Just (FrontendSessionLoginData tok uid (Just DashboardTabDetails))
        app <- serveFAction (Proxy :: Proxy TestApi) (testApi mfsl) st
        runWaiSession test app) `finally` destroyAllResources connPool
