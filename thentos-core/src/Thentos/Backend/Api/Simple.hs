{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TypeFamilies                             #-}
{-# LANGUAGE TypeOperators                            #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Thentos.Backend.Api.Simple where

import Network.Wai (Application)
import Servant.API.Header (Header)
import Servant.API ((:<|>)((:<|>)), (:>), Get, Post, Delete, Capture, ReqBody, JSON)
import Servant.Server (ServerT, Server, serve, enter)
import Servant.API.ResponseHeaders (Headers, addHeader)

import qualified Servant.Docs as Docs

import Thentos.Action
import Thentos.Action.Types (MonadQuery, MonadAction, ActionEnv, aStConfig)
import Thentos.Backend.Api.Auth
import Thentos.Backend.Api.Docs.Common
import Thentos.Backend.Core
import Thentos.Config
import Thentos.Ends.Types
import Thentos.Prelude
import Thentos.Types

import qualified Paths_thentos_core__ as Paths (version)
import qualified Thentos.Backend.Api.PureScript as Purs


-- * main

runApi :: HttpConfig -> ActionEnv -> IO ()
runApi cfg asg = do
    logger INFO $ "running rest api Thentos.Backend.Api.Simple on " ++ show (bindUrl cfg) ++ "."
    runWarpWithCfg cfg $ serveApi cfg asg

serveApi :: HttpConfig -> ActionEnv -> Application
serveApi cfg astate = addCacheControlHeaders $
    let p = Proxy :: Proxy (RestDocs Api)
    in serve p (restDocs cfg p :<|> api astate)

type Api =
       ThentosAssertHeaders :> ThentosAuth :> ThentosBasic
  :<|> "js" :> Purs.Api

api :: ActionEnv -> Server Api
api as =
       (\creds -> enter (enterAction () as baseActionErrorToServantErr creds) thentosBasic)
  :<|> Purs.api (as ^. aStConfig)


-- * combinators

type ThentosBasic =
       "user" :> ThentosUser
  :<|> "service" :> ThentosService
  :<|> "thentos_session" :> ThentosThentosSession
  :<|> "service_session" :> ThentosServiceSession
  :<|> "email" :> SendEmail

thentosBasic :: MonadAction e m => ServerT ThentosBasic m
thentosBasic =
       thentosUser
  :<|> thentosService
  :<|> thentosThentosSession
  :<|> thentosServiceSession
  :<|> sendEmail


-- * user

type ThentosUser =
       ReqBody '[JSON] UserFormData :> Post '[JSON] (JsonTop UserId)
       -- register returns 204 No Content
       -- FIXME We should use '[] instead of '[JSON] as result type for UserCreationRequest,
       -- but that causes a compile error at "serve p (restDocs ..."
  :<|> "register" :> ReqBody '[JSON] UserCreationRequest :> Post '[JSON] ()
  :<|> "activate" :> ReqBody '[JSON] (JsonTop ConfirmationToken)
                  :> Post '[JSON] (JsonTop ThentosSessionToken)
  :<|> "login" :> ReqBody '[JSON] LoginFormData :> Post '[JSON] (JsonTop ThentosSessionToken)
  :<|> Capture "uid" UserId :> Delete '[JSON] ()
  :<|> Capture "uid" UserId :> "name" :> Get '[JSON] (JsonTop UserName)
  :<|> Capture "uid" UserId :> "email" :> Get '[JSON] (JsonTop UserEmail)
  :<|> "captcha" :> Post '[PNG] (Headers '[Header "Thentos-Captcha-Id" CaptchaId] ImageData)
  :<|> "audio_captcha" :> Capture "voice" ST
          :> Post '[WAV] (Headers '[Header "Thentos-Captcha-Id" CaptchaId] SBS)
  :<|> "create_password_reset" :> ReqBody '[JSON] WrappedEmail :> Post '[JSON] ()
  :<|> "password_reset" :> ReqBody '[JSON] PasswordResetRequest
                        :> Post '[JSON] (JsonTop ThentosSessionToken)

thentosUser :: MonadAction e m => ServerT ThentosUser m
thentosUser =
       (JsonTop <$>) . addUser
  :<|> addUnconfirmedUserWithCaptcha
  :<|> (JsonTop <$>) . (snd <$>) .  confirmNewUser . fromJsonTop
  :<|> (\(LoginFormData n p) -> JsonTop . snd <$> startThentosSessionByUserName n p)
  :<|> deleteUser
  :<|> (JsonTop . ((^. userName) . snd) <$>) . lookupConfirmedUser
  :<|> (JsonTop . ((^. userEmail) . snd) <$>) . lookupConfirmedUser
  :<|> (makeCaptcha >>= \(cid, img) -> return $ addHeader cid img)
  :<|> (\voice -> makeAudioCaptcha (cs voice) >>= \(cid, wav) -> return $ addHeader cid wav)
  :<|> (\(WrappedEmail e) -> sendPasswordResetMail Nothing e)
  :<|> (JsonTop <$>) . (\(PasswordResetRequest tok pass) -> resetPasswordAndLogin tok pass)


-- * service

type ThentosService =
       ReqBody '[JSON] (UserId, ServiceName, ServiceDescription) :> Post '[JSON] (ServiceId, ServiceKey)
           -- FIXME: it would be much nicer to infer the owner from
           -- the session token, but that requires changes to the
           -- various action monads we are kicking around all over the
           -- place.  coming up soon!

  :<|> Capture "sid" ServiceId :> Delete '[JSON] ()
  :<|> Get '[JSON] [ServiceId]

thentosService :: MonadAction e m => ServerT ThentosService m
thentosService =
         (\ (uid, sn, sd) -> addService uid sn sd)
    :<|> deleteService
    :<|> allServiceIds


-- * session

type ThentosThentosSession =
       ReqBody '[JSON] ByUserOrServiceId   :> Post '[JSON] ThentosSessionToken
  :<|> ReqBody '[JSON] ThentosSessionToken :> Get '[JSON] Bool
  :<|> ReqBody '[JSON] ThentosSessionToken :> Delete '[JSON] ()

thentosThentosSession :: MonadAction e m => ServerT ThentosThentosSession m
thentosThentosSession =
       startThentosSession
  :<|> existsThentosSession
  :<|> endThentosSession
  where startThentosSession (ByUser id' pass) = startThentosSessionByUserId id' pass
        startThentosSession (ByService id' key) = startThentosSessionByServiceId id' key


-- * service session

type ThentosServiceSession =
       ReqBody '[JSON] ServiceSessionToken :> Get '[JSON] Bool
  :<|> ReqBody '[JSON] ServiceSessionToken :> "meta" :> Get '[JSON] ServiceSessionMetadata
  :<|> ReqBody '[JSON] ServiceSessionToken :> Delete '[JSON] ()

thentosServiceSession :: MonadQuery e m => ServerT ThentosServiceSession m
thentosServiceSession =
       existsServiceSession
  :<|> getServiceSessionMetadata
  :<|> endServiceSession


-- * send email

type SendEmail =
    ReqBody '[JSON] SendEmailRequest :> Post '[JSON] ()


-- * servant docs

instance HasDocExtras (RestDocs Api) where
    getCabalPackageName _ = "thentos-core"
    getCabalPackageVersion _ = Paths.version

    getTitle _ = "The thentos API family: Core"

    getIntros _ =
        [ Docs.DocIntro "@@0.2@@Overview" [unlines $
            [ "`Core` is a simple, general-purpose user management protocol"
            , "that supports using one identity for multiple services.  It has"
            , "all the expected basic features like email confirmation, password"
            , "reset, change of user data.  Furthermore, it allows to create services,"
            , "register users with services, and manage the user's service login"
            , "sessions."
            ]]]

    getExtraInfo _ = mconcat
        [ Docs.extraInfo (Proxy :: Proxy (ThentosAssertHeaders :> ThentosAuth :>
                                            "service" :> Get '[JSON] [ServiceId]))
                $ Docs.defAction & Docs.notes <>~
            [Docs.DocNote "delete a service and unregister all its users" []]
        ]
