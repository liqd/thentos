{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Thentos.Frontend where

import Control.Applicative ((<$>), (<*>))
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
import Thentos.Frontend.Handlers as H
import Thentos.Frontend.Util (serveSnaplet)
import Thentos.Frontend.Types

runFrontend :: HttpConfig -> ActionStateGlobal (MVar SystemRNG) -> IO ()
runFrontend config asg = do
    logger INFO $ "running frontend on " <> show (bindUrl config) <> "."
    serveSnaplet (setBind host $ setPort port defaultConfig) (frontendApp asg)
  where
    host :: ByteString = cs $ config >>. (Proxy :: Proxy '["bind_host"])
    port :: Int = config >>. (Proxy :: Proxy '["bind_port"])

frontendApp :: ActionStateGlobal (MVar SystemRNG) -> SnapletInit FrontendApp FrontendApp
frontendApp (st, rn, _cfg) = makeSnaplet "Thentos" "The Thentos universal user management system" Nothing $ do
    addRoutes routes
    FrontendApp <$>
        (nestSnaplet "acid" db $ acidInitManual st) <*>
        (return rn) <*>
        (return _cfg) <*>
        (nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600))

routes :: [(ByteString, Handler FrontendApp FrontendApp ())]
routes = [ ("", ifTop $ mainPageHandler)

         , ("log_into_thentos", logIntoThentosHandler)

         -- FIXME: make "user/" a sub-routing-table.
         , ("user_create", userAddHandler)
         , ("user_create_confirm", userAddConfirmHandler)
         , ("user_reset_password_request", requestPasswordResetHandler)
         , ("user_reset_password", resetPasswordHandler)
         -- , ("user_update", ?)

         , ("service_create", method GET addServiceHandler)
         , ("service_create", method POST addServiceHandler)

         , ("log_into_service", logIntoServiceHandler)
         , ("check_thentos_login", checkThentosLoginHandler)
         ]
