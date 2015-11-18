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

import System.Log.Missing (logger)
import Thentos.Action.Core
import Thentos.Backend.Core
import Thentos.Config
import Thentos.Frontend.Handlers
import Thentos.Frontend.Handlers.Combinators
import Thentos.Frontend.State
import Thentos.Frontend.TH
import Thentos.Frontend.Types


-- * driver

runFrontend :: HttpConfig -> ActionState -> IO ()
runFrontend config aState = do
    logger INFO $ "running frontend on " ++ show (bindUrl config) ++ "."
    serveFAction (Proxy :: Proxy FrontendH) frontendH aState >>= runWarpWithCfg config . disableCaching

type FrontendH =
       Get '[HTM] H.Html
  :<|> "user" :> UserH
  :<|> "service" :> ServiceH
  :<|> "dashboard" :> DashboardH
  :<|> StaticContent

frontendH :: ServerT FrontendH FAction
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

staticContent :: forall m. Applicative m => ServerT StaticContent m
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

userH :: ServerT UserH FAction
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

serviceH :: ServerT ServiceH FAction
serviceH =
       serviceLoginH
  :<|> serviceRegisterH
  :<|> serviceCreateH
