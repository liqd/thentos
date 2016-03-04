{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}

module Thentos.Frontend (runFrontend) where

import Data.String.Conversions (LBS)
import Servant hiding (serveDirectory)
import System.Log.Logger (Priority(INFO))

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Network.Wai as Wai

import System.Log.Missing (logger)
import Thentos.Action.Types (ActionEnv)
import Thentos.Backend.Core (addHeadersToResponse, runWarpWithCfg)
import Thentos.Config
import Thentos.Ends.Types
import Thentos.Frontend.Handlers
import Thentos.Frontend.Handlers.Combinators
import Thentos.Frontend.State
import Thentos.Frontend.TH
import Thentos.Frontend.Types


-- * driver

runFrontend :: HttpConfig -> ActionEnv -> IO ()
runFrontend config aState = do
    logger INFO $ "running frontend on " ++ show (bindUrl config) ++ "."
    serveFAction (Proxy :: Proxy FrontendH) frontendH aState >>= runWarpWithCfg config . disableCaching

type FrontendH =
       GetH
  :<|> "user" :> UserH
  :<|> "service" :> ServiceH
  :<|> "dashboard" :> DashboardH
  :<|> StaticContent

frontendH :: FormHandler (ServerT FrontendH)
frontendH =
       redirect' "/dashboard"
  :<|> userH
  :<|> serviceH
  :<|> dashboardH
  :<|> staticContent


-- * static content

-- | Instead of ServeDirectory, we bake all static content into the executable.  This helps to
-- minimize the number of moving parts in the deployment.
type StaticContent =
       "screen.css" :> Get '[TextCss] LBS

staticContent :: Applicative m => ServerT StaticContent m
staticContent =
       l $(loadStaticContent "screen.css")
  where
    l = pure . LBS.pack


-- * /user

type UserH =
       UserRegisterH
  :<|> UserRegisterConfirmH
  :<|> UserLoginH
  :<|> ResetPasswordRequestH
  :<|> ResetPasswordH
  :<|> UserLogoutH
  :<|> EmailUpdateH
  :<|> EmailUpdateConfirmH
  :<|> PasswordUpdateH

userH :: FormHandler (ServerT UserH)
userH =
       userRegisterH
  :<|> userRegisterConfirmH
  :<|> userLoginH
  :<|> resetPasswordRequestH
  :<|> resetPasswordH
  :<|> userLogoutH
  :<|> emailUpdateH
  :<|> emailUpdateConfirmH
  :<|> passwordUpdateH


-- * service

type ServiceH =
       ServiceLoginH
  :<|> ServiceRegisterH
  :<|> ServiceCreateH

serviceH :: FormHandler (ServerT ServiceH)
serviceH =
       serviceLoginH
  :<|> serviceRegisterH
  :<|> serviceCreateH


-- * Cache control

-- | Disable response caching. The wrapped handler can overwrite this by
-- setting its own cache control headers.
--
-- Cache-control headers are only added to GET and HEAD responses since other request methods
-- are considered uncachable by default.
--
-- According to the HTTP 1.1 Spec, GET/HEAD responses with the following error codes (>= 400) may
-- be cached unless forbidded by cache-control headers:
--
-- * 404 Not Found
-- * 405 Method Not Allowed
-- * 410 Gone
-- * 414 Request-URI Too Long
-- * 501 Not Implemented
disableCaching :: Wai.Middleware
disableCaching app req cont = app req $
    cont . (if relevantMeth then addHeadersToResponse cacheHeaders else id)
  where
    cacheHeaders =
        [ ("Cache-Control", "no-cache, no-store, must-revalidate")
        , ("Expires", "0")
        ]

    relevantMeth :: Bool
    relevantMeth = Wai.requestMethod req `elem` ["GET", "HEAD"]
