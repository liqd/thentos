{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Thentos.Frontend where

import Control.Applicative (pure, (<$>), (<*>))
import Data.ByteString (ByteString)
import Data.Configifier ((>>.))
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (cs)
import Snap.Core (ifTop, redirect')
import Snap.Http.Server (defaultConfig, setBind, setPort, setVerbose)
import Snap.Snaplet.Session.Backends.CookieSession (initCookieSessionManager)
import Snap.Snaplet (SnapletInit, makeSnaplet, nestSnaplet, addRoutes, wrapSite)
import Snap.Util.FileServe (serveDirectory)
import System.Log.Missing (logger)
import System.Log (Priority(INFO))

import qualified Thentos.Frontend.Handlers as H
import qualified Thentos.Frontend.Pages as P

import Snap.Missing (serveSnaplet)
import Thentos.Action.Core
import Thentos.Config
import Thentos.Frontend.Handlers.Combinators
import Thentos.Frontend.Types
import Thentos.Types


runFrontend :: HttpConfig -> ActionState -> IO ()
runFrontend config asg = do
    logger INFO $ "running frontend on " <> show (bindUrl config) <> "."
    serveSnaplet snapConfig (frontendApp asg config)
  where
    host :: ByteString = cs $ config >>. (Proxy :: Proxy '["bind_host"])
    port :: Int = config >>. (Proxy :: Proxy '["bind_port"])
    snapConfig = setPort port
               . setBind host
               . setVerbose False
               $ defaultConfig

frontendApp :: ActionState -> HttpConfig -> SnapletInit FrontendApp FrontendApp
frontendApp (ActionState (st, rn, _cfg)) feConf =
    makeSnaplet "Thentos" "The Thentos universal user management system" Nothing $ do
        wrapSite H.disableCaching
        wrapSite csrfify
        addRoutes routes
        FrontendApp <$>
            return () <*>
            return rn <*>
            return _cfg <*>
            nestSnaplet "sess" sess
               (initCookieSessionManager "site_key.txt" "sess" (Just 3600)) <*>
            pure feConf

routes :: [(ByteString, FH ())]
routes = [ -- default entry point
           ("", ifTop $ redirect' "/dashboard" 303)

           -- if not logged in
         , ("user/register", H.userRegister)
         , ("user/register_confirm", H.userRegisterConfirm)
         , ("user/login", H.userLogin)
         , ("user/reset_password_request", H.resetPassword)
         , ("user/reset_password", H.resetPasswordConfirm)

           -- if logged in
         , ("user/logout", H.userLogout)
         , ("user/update", H.userUpdate)
         , ("user/update_email", H.emailUpdate)
         , ("user/update_email_confirm", H.emailUpdateConfirm)
         , ("user/update_password", H.passwordUpdate)

         , ("service/register", H.serviceRegister)
         , ("service/login", H.serviceLogin)

         , ("/dashboard", redirect' "/dashboard/details" 303)
             -- (this could also look up the last page the user was
             -- on, and navigate there.)

             -- (dashboard should probably be a snaplet.  if only for
             -- the routing table and the call to the
             -- dashboardPagelet.)

         , ("/dashboard/details",     renderDashboard P.DashboardTabDetails P.userDisplayPagelet)
         , ("/dashboard/services",    renderDashboard P.DashboardTabServices $ P.userServicesDisplayPagelet)
         , ("/dashboard/ownservices", H.serviceCreate)
         , ("/dashboard/users",       renderDashboard P.DashboardTabUsers $ \ _ _ -> "nothing here yet!")

         -- static files
         , ("", serveDirectory "snap/static")

         -- 404 error page if no other route exists
         , ("", H.unknownPath)
         ]
