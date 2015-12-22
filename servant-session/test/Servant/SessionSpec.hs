{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Servant.SessionSpec (spec) where

import           Control.Monad              (replicateM_)
import           Control.Monad.Trans.Except (ExceptT)
import qualified Data.Vault.Lazy            as Vault
import           Network.HTTP.Types         (methodGet)
import           Network.Wai                (Middleware, Application)
import           Network.Wai.Session        (SessionStore, Session, withSession)
import           Network.Wai.Session.Map    (mapStore)
import           Network.Wai.Test           (simpleBody, simpleHeaders)
import           Servant                    (Proxy(Proxy), ServantErr, Get, JSON, (:>), serve)
import           Test.Hspec                 (Spec, context, describe, it, shouldBe, shouldSatisfy)
import           Test.Hspec.Wai             (with, request, liftIO)
import           Web.Cookie                 (SetCookie, def, parseSetCookie,
                                             setCookieName, setCookieValue, setCookieMaxAge)

import           Servant.Session


spec :: Spec
spec = describe "Servant.Session" . with server $ do

  context "the cookie is set" $ do

    it "has read and write access to the cookie" $ do
        replicateM_ 5 $ request methodGet "" [("Cookie", "test=const")] ""
        x <- request methodGet "" [("Cookie", "test=const")] ""
        liftIO $ simpleBody x `shouldSatisfy` (== "\"4\"")


  context "no cookie is set" $ do

    it "one will be in the Set-Cookie header of the response" $ do
        resp <- request methodGet "" [] ""
        let Just c = parseSetCookie <$> lookup "Set-Cookie" (simpleHeaders resp)
        liftIO $ setCookieName c `shouldBe` setCookieName setCookieOpts
        liftIO $ setCookieValue c `shouldBe` "const"

    it "adds SetCookie params" $ do
        resp <- request methodGet "" [] ""
        let Just c = parseSetCookie <$> lookup "Set-Cookie" (simpleHeaders resp)
        liftIO $ setCookieMaxAge c `shouldBe` setCookieMaxAge setCookieOpts


type API = SSession IO Int Int :> Get '[JSON] String

setCookieOpts :: SetCookie
setCookieOpts = def { setCookieName = "test", setCookieMaxAge = Just 300 }

sessionMiddleware :: SessionStore IO Int a -> Vault.Key (Session IO Int a) -> Middleware
sessionMiddleware s = withSession s "test" setCookieOpts

server :: IO Application
server = do
    ref <- mapStore (return "const")
    key <- Vault.newKey
    return $ sessionMiddleware ref key
           $ serve (Proxy :: Proxy API) (handler key)

handler :: Vault.Key (Session IO Int Int)
        -> (Vault.Key (Session IO Int Int) -> Maybe (Session IO Int Int))
        -> ExceptT ServantErr IO String
handler key smap = do
    x <- liftIO $ lkup 1
    case x of
        Nothing -> liftIO (ins 1 0) >> return "Nothing"
        Just y -> liftIO (ins 1 $ succ y) >> return (show y)
  where
    Just (lkup, ins) = smap key
