{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This is an implementation of
-- git@github.com:liqd/adhocracy3.git:/docs/source/api/authentication_api.rst
module Thentos.Adhocracy3.Backend.Api.Simple
    ( A3Resource(..)
    , A3UserNoPass(..)
    , A3UserWithPass(..)
    , ActivationRequest(..)
    , Api
    , ContentType(..)
    , LoginRequest(..)
    , PasswordResetRequest(..)
    , Path(..)
    , RequestResult(..)
    , ThentosApi
    , TypedPath(..)
    , TypedPathWithCacheControl(..)
    , a3corsPolicy
    , a3ProxyAdapter
    , runBackend
    , serveApi
    , thentosApi
    ) where

import Control.Lens ((&), (<>~))
import Data.CaseInsensitive (mk)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (SBS, ST, cs)
import Network.Socket (SockAddr(SockAddrInet))
import Network.Wai (Application)
import Servant.API ((:<|>)((:<|>)), (:>), Capture, ReqBody, JSON, Post)
import Servant.API.Header (Header)
import Servant.API.ResponseHeaders (Headers, addHeader)
import Servant.Docs (ToSample(toSamples))
import Servant.Server.Internal (Server, ServerT)
import Servant.Server (serve, enter)
import System.Log (Priority(INFO))

import qualified Network.HTTP.Client as Client
import qualified Servant.Docs as Docs

import System.Log.Missing
import Thentos.Adhocracy3.Action
import Thentos.Adhocracy3.Action.Types
import Thentos.Adhocracy3.Backend.Core
import Thentos.Backend.Api.Auth.Types
import Thentos.Backend.Api.Docs.Common
    ( RestDocs, restDocs
    , HasDocExtras(getCabalPackageName, getCabalPackageVersion, getTitle, getIntros, getExtraInfo)
    )
import Thentos.Backend.Api.Docs.Proxy ()
import Thentos.Backend.Api.Proxy
import Thentos.Backend.Core
import Thentos.Config
import Thentos.Ends.Types (PNG, WAV)
import Thentos.Types

import qualified Paths_thentos_adhocracy__ as Paths (version)
import qualified Thentos.Action as A
import qualified Thentos.Action.Types as AC
import qualified Thentos.Backend.Api.PureScript


-- * main

runBackend :: HttpConfig -> AC.ActionState -> IO ()
runBackend cfg asg = do
    logger INFO $ "running rest api (a3 style) on " ++ show (bindUrl cfg) ++ "."
    manager <- Client.newManager Client.defaultManagerSettings
    runWarpWithCfg cfg $ serveApi manager cfg asg

serveApi :: Client.Manager -> HttpConfig -> AC.ActionState -> Application
serveApi manager beConfig astate = addCorsHeaders a3corsPolicy . addCacheControlHeaders $
    let p = Proxy :: Proxy (RestDocs Api)
    in serve p (restDocs beConfig p :<|> api manager astate)


-- * api

-- | Note: login_username and login_email have identical behavior.  In
-- particular, it is not an error to send username and password to
-- @/login_email@.  This makes implementing all sides of the protocol
-- a lot easier without sacrificing security.
type ThentosApi =
       "principals" :> "users" :> ReqBody '[JSON] A3UserWithPass
                               :> Post200 '[JSON] TypedPathWithCacheControl
  :<|> "activate_account"      :> ReqBody '[JSON] ActivationRequest
                               :> Post200 '[JSON] RequestResult
  :<|> "login_username"        :> ReqBody '[JSON] LoginRequest
                               :> Post200 '[JSON] RequestResult
  :<|> "login_email"           :> ReqBody '[JSON] LoginRequest
                               :> Post200 '[JSON] RequestResult
  :<|> "password_reset"        :> ReqBody '[JSON] PasswordResetRequest
                               :> Post200 '[JSON] RequestResult
  :<|> "thentos" :> "user" :> ThentosApiWithWidgets

type ThentosApiWithWidgets =
       "register" :> ReqBody '[JSON] UserCreationRequest :> Post '[JSON] ()
  :<|> "activate" :> ReqBody '[JSON] (JsonTop ConfirmationToken)
                  :> Post '[JSON] (JsonTop ThentosSessionToken)
  :<|> "captcha"  :> Post '[PNG] (Headers '[Header "Thentos-Captcha-Id" CaptchaId] ImageData)
  :<|> "audio_captcha" :> Capture "voice" ST
          :> Post '[WAV] (Headers '[Header "Thentos-Captcha-Id" CaptchaId] SBS)

type Api =
       ThentosApi
  :<|> "js" :> Thentos.Backend.Api.PureScript.Api
  :<|> ServiceProxy

-- | thentos-adhocracy's "Simple" api does not use thentos-core's "Auth" functionality, which is
-- baked into the 'Action' monad.  'emptyCreds' is a credential value that can be passed into
-- 'enter' that won't raise the clearance level for anybody.
--
-- FIXME: there really should be a better structure for all this.  either "Auth" is too specific to
-- be baked into 'Action', or it should be general enough to be used here (see comment on
-- 'renderThentosHeaderName').  (Also consider replacing "Auth" with
-- https://github.com/haskell-servant/servant/pull/185 once it's ready.)
emptyCreds :: ThentosAuthCredentials
emptyCreds = ThentosAuthCredentials Nothing (SockAddrInet 0 0)

thentosApi :: AC.ActionState -> Server ThentosApi
thentosApi as = enter (enterAction () as a3ActionErrorToServantErr emptyCreds) $
       addUser
  :<|> activate
  :<|> login
  :<|> login
  :<|> resetPassword
  :<|> thentosApiWithWidgets

thentosApiWithWidgets :: ServerT ThentosApiWithWidgets A3Action
thentosApiWithWidgets =
       A.addUnconfirmedUserWithCaptcha
  :<|> (JsonTop <$>) . (snd <$>) .  A.confirmNewUser . fromJsonTop
  :<|> (A.makeCaptcha >>= \(cid, img) -> return $ addHeader cid img)
  :<|> (\voice -> A.makeAudioCaptcha (cs voice) >>= \(cid, wav) -> return $ addHeader cid wav)


api :: Client.Manager -> AC.ActionState -> Server Api
api manager actionState@(AC.ActionState cfg _ _) =
       thentosApi actionState
  :<|> Thentos.Backend.Api.PureScript.api cfg
  :<|> serviceProxy manager a3ProxyAdapter actionState


-- * policy

a3corsPolicy :: CorsPolicy
a3corsPolicy = CorsPolicy
    { corsHeaders = "Origin, Content-Type, Accept, X-User-Path, X-User-Token"
    , corsMethods = "POST,GET,DELETE,PUT,OPTIONS"
    , corsOrigin  =  "*"
    }

-- | A3-specific ProxyAdapter.
a3ProxyAdapter :: ProxyAdapter ThentosA3Error
a3ProxyAdapter = ProxyAdapter
  { renderHeader = renderA3HeaderName
  , renderUser   = a3RenderUser
  , renderError  = a3ActionErrorToServantErr
  }

-- | Render Thentos/A3-specific custom headers using the names expected by A3.
renderA3HeaderName :: RenderHeaderFun
renderA3HeaderName ThentosHeaderSession = mk "X-User-Token"
renderA3HeaderName ThentosHeaderUser    = mk "X-User-Path"
renderA3HeaderName h                    = renderThentosHeaderName h

-- | Render the user as A3 expects it. We return the external URL of the user's default persona.
a3RenderUser :: UserId -> User -> A3Action SBS
a3RenderUser uid _ = externalUrlOfDefaultPersona uid


-- * servant docs

instance HasDocExtras (RestDocs Api) where
    getCabalPackageName _ = "thentos-adhocracy"
    getCabalPackageVersion _ = Paths.version
    getTitle _ = "The thentos API family: Adhocracy3 Proxy"
    getIntros _ =
        [ Docs.DocIntro "@@0.2@@Overview" [unlines $
            [ "Adhocracy3 has a basic user management built-in.  In order for thentos"
            , "to have minimal impact on the existing code base, it can be deployed"
            , "as a reverse proxy and mimic the built-in user management rest api."
            , "This way, the frontend does not need to change at all to use the old"
            , "features of the new user management system.  The impact of new"
            , "features to the frontend can be kept at a minimum."
            , ""
            , "What follows is the fully compatible adhocracy3 user management rest"
            , "api.  Any deviation should be considered an error and reported in a"
            , "later version of this document."
            ]]]

    getExtraInfo _ = mconcat
        [ Docs.extraInfo (Proxy :: Proxy ("principals" :> "users" :> ReqBody '[JSON] A3UserWithPass
                               :> Post200 '[JSON] TypedPathWithCacheControl))
                $ Docs.defAction & Docs.notes <>~
            [ Docs.DocNote "request creation of a new account" [unlines $
                [ "When the user-creation form is filled out with login name, email, and"
                , "password, this end-point is used to post the form content and trigger"
                , "the email confirmation procedure."
                ]
            ]]
        , Docs.extraInfo (Proxy :: Proxy ("activate_account" :> ReqBody '[JSON] ActivationRequest
                               :> Post200 '[JSON] RequestResult))
                $ Docs.defAction & Docs.notes <>~
            [ Docs.DocNote "email-click account activation" [unlines $
                [ "The confirmation email contains a link to this end-point."
                , "The path contains a token can only be learned from receiving"
                , "(or intercepting) the email."
                ]
            ]]
        , Docs.extraInfo (Proxy :: Proxy ("login_username" :> ReqBody '[JSON] LoginRequest
                               :> Post200 '[JSON] RequestResult))
                $ Docs.defAction & Docs.notes <>~
            [ Docs.DocNote "login with user name" []
            ]
        , Docs.extraInfo (Proxy :: Proxy ("login_email" :> ReqBody '[JSON] LoginRequest
                               :> Post200 '[JSON] RequestResult))
                $ Docs.defAction & Docs.notes <>~
            [ Docs.DocNote "login with user email" []
            ]
        , Docs.extraInfo (Proxy :: Proxy ("password_reset" :> ReqBody '[JSON] PasswordResetRequest
                               :> Post200 '[JSON] RequestResult))
                $ Docs.defAction & Docs.notes <>~
            [ Docs.DocNote "reset password" []
            ]
        ]

instance ToSample A3UserNoPass

instance ToSample A3UserWithPass

instance ToSample a => ToSample (A3Resource a)

instance ToSample TypedPath

instance ToSample Path where
    toSamples _ = Docs.singleSample $ Path "/proposals/environment"

instance ToSample TypedPathWithCacheControl

instance ToSample ActivationRequest

-- FIXME: split up LoginRequest into two separate types for login by email
-- and login by user name, in order to provide a correct example for
-- login_email request body
instance ToSample LoginRequest

instance ToSample RequestResult where
    toSamples _ = [ ("Success", RequestSuccess (Path "somepath") "sometoken")]

instance ToSample PasswordResetRequest

instance ToSample ContentType where
    toSamples _ = Docs.singleSample CTUser
