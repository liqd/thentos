{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Thentos.Frontend (runFrontend) where

import Data.String.Conversions (LBS)
import Servant hiding (serveDirectory)
import System.Log.Logger (Priority(INFO))

import qualified Text.Blaze.Html5 as H
import qualified Data.ByteString.Lazy.Char8 as LBS

import Thentos.Action.Core
import Thentos.Backend.Core
import Thentos.Config
import Thentos.Frontend.Handlers
import Thentos.Frontend.Handlers.Combinators
import Thentos.Frontend.State
import Thentos.Frontend.TH
import Thentos.Frontend.Types

import qualified System.Log.Missing as Log (logger)



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
        FrontendApp st rn _cfg <$>
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
