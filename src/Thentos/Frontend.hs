{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Thentos.Frontend where

import Control.Applicative (pure, (<$>), (<*>))
import Control.Concurrent.MVar (MVar)
import Crypto.Random (SystemRNG)
import Data.ByteString (ByteString)
import Data.Configifier ((>>.))
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (cs)
import Snap.Core (method, Method(GET, POST), ifTop)
import Snap.Http.Server (defaultConfig, setBind, setPort)
import Snap.Snaplet.AcidState (acidInitManual)
import Snap.Snaplet.Session.Backends.CookieSession (initCookieSessionManager)
import Snap.Snaplet (SnapletInit, makeSnaplet, nestSnaplet, addRoutes, Handler)
import System.Log (Priority(INFO))
import System.Log.Missing (logger)

import Thentos.Api
import Thentos.Config
import Thentos.Frontend.Util (serveSnaplet)
import Thentos.Frontend.Types

import qualified Thentos.Frontend.Handlers as H

runFrontend :: HttpConfig -> ActionStateGlobal (MVar SystemRNG) -> IO ()
runFrontend config asg = do
    logger INFO $ "running frontend on " <> show (bindUrl config) <> "."
    serveSnaplet (setBind host $ setPort port defaultConfig) (frontendApp asg config)
  where
    host :: ByteString = cs $ config >>. (Proxy :: Proxy '["bind_host"])
    port :: Int = config >>. (Proxy :: Proxy '["bind_port"])

frontendApp :: ActionStateGlobal (MVar SystemRNG) -> HttpConfig -> SnapletInit FrontendApp FrontendApp
frontendApp (st, rn, _cfg) feConf =
    makeSnaplet "Thentos" "The Thentos universal user management system" Nothing $ do
        addRoutes routes
        FrontendApp <$>
            (nestSnaplet "acid" db $ acidInitManual st) <*>
            (return rn) <*>
            (return _cfg) <*>
            (nestSnaplet "sess" sess $
               initCookieSessionManager "site_key.txt" "sess" (Just 3600)) <*>
            (pure feConf)

routes :: [(ByteString, Handler FrontendApp FrontendApp ())]
routes = [ ("", ifTop $ H.index)

         , ("login_thentos", H.loginThentos)
         , ("user/create", H.userCreate)
         , ("user/create_confirm", H.userCreateConfirm)
         , ("user/reset_password_request", H.resetPasswordRequest)
         , ("user/reset_password", H.resetPassword)
         -- , ("user/update", ?)

         , ("service/create", method GET H.serviceCreate)
         , ("service/create", method POST H.serviceCreate)

         , ("login_service", H.loginService)
         , ("check_thentos_login", H.checkThentosLogin)  -- FIXME: what is this used for?  drop it?
         ]
