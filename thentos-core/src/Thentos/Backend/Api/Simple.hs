{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE ExistentialQuantification                #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE InstanceSigs                             #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TupleSections                            #-}
{-# LANGUAGE TypeFamilies                             #-}
{-# LANGUAGE TypeOperators                            #-}
{-# LANGUAGE TypeSynonymInstances                     #-}
{-# LANGUAGE UndecidableInstances                     #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Thentos.Backend.Api.Simple where

import Control.Lens ((^.), (&), (<>~))
import Data.Proxy (Proxy(Proxy))
import Data.Void (Void)
import Network.Wai (Application)
import Servant.API ((:<|>)((:<|>)), (:>), Get, Post, Delete, Capture, ReqBody, JSON)
import Servant.Server (ServerT, Server, serve, enter)
import System.Log.Logger (Priority(INFO))

import qualified Servant.Docs as Docs

import System.Log.Missing (logger)
import Thentos.Action
import Thentos.Action.Core (ActionState(ActionState), Action)
import Thentos.Backend.Api.Auth
import Thentos.Backend.Api.Docs.Common
import Thentos.Backend.Core
import Thentos.Config
import Thentos.Types

import qualified Paths_thentos_core__ as Paths
import qualified Thentos.Backend.Api.Purescript as Purs


-- * main

runApi :: HttpConfig -> ActionState -> IO ()
runApi cfg asg = do
    logger INFO $ "running rest api Thentos.Backend.Api.Simple on " ++ show (bindUrl cfg) ++ "."
    runWarpWithCfg cfg $ serveApi asg

serveApi :: ActionState -> Application
serveApi astate = addCacheControlHeaders $
    let p = Proxy :: Proxy (RestDocs Api)
    in serve p (pure (restDocs p) :<|> api astate)

type Api =
       ThentosAssertHeaders :> ThentosAuth :> ThentosBasic
  :<|> "js" :> Purs.Api

api :: ActionState -> Server Api
api actionState@(ActionState (_, _, cfg)) =
       (\mTok -> enter (enterAction () actionState baseActionErrorToServantErr mTok) thentosBasic)
  :<|> Purs.api cfg

-- * combinators

type ThentosBasic =
       "user" :> ThentosUser
  :<|> "service" :> ThentosService
  :<|> "thentos_session" :> ThentosThentosSession
  :<|> "service_session" :> ThentosServiceSession

thentosBasic :: ServerT ThentosBasic (Action Void ())
thentosBasic =
       thentosUser
  :<|> thentosService
  :<|> thentosThentosSession
  :<|> thentosServiceSession


-- * user

type ThentosUser =
       ReqBody '[JSON] UserFormData :> Post '[JSON] (JsonTop UserId)
  :<|> "register" :> ReqBody '[JSON] UserCreationRequest :> Post '[JSON] (JsonTop UserId)
  :<|> "activate" :> ReqBody '[JSON] (JsonTop ConfirmationToken)
                  :> Post '[JSON] (JsonTop ThentosSessionToken)
  :<|> "login" :> ReqBody '[JSON] LoginFormData :> Post '[JSON] (JsonTop ThentosSessionToken)
  :<|> Capture "uid" UserId :> Delete '[JSON] ()
  :<|> Capture "uid" UserId :> "name" :> Get '[JSON] (JsonTop UserName)
  :<|> Capture "uid" UserId :> "email" :> Get '[JSON] (JsonTop UserEmail)

thentosUser :: ServerT ThentosUser (Action Void ())
thentosUser =
       (JsonTop <$>) . addUser
  :<|> (JsonTop <$>) . addUnconfirmedUserWithCaptcha
  :<|> (JsonTop <$>) . (snd <$>) .  confirmNewUser . fromJsonTop
  :<|> (\(LoginFormData n p) -> JsonTop . snd <$> startThentosSessionByUserName n p)
  :<|> deleteUser
  :<|> (JsonTop . ((^. userName) . snd) <$>) . lookupConfirmedUser
  :<|> (JsonTop . ((^. userEmail) . snd) <$>) . lookupConfirmedUser


-- * service

type ThentosService =
       ReqBody '[JSON] (UserId, ServiceName, ServiceDescription) :> Post '[JSON] (ServiceId, ServiceKey)
           -- FIXME: it would be much nicer to infer the owner from
           -- the session token, but that requires changes to the
           -- various action monads we are kicking around all over the
           -- place.  coming up soon!

  :<|> Capture "sid" ServiceId :> Delete '[JSON] ()
  :<|> Get '[JSON] [ServiceId]

thentosService :: ServerT ThentosService (Action Void ())
thentosService =
         (\ (uid, sn, sd) -> addService uid sn sd)
    :<|> deleteService
    :<|> allServiceIds


-- * session

type ThentosThentosSession =
       ReqBody '[JSON] ByUserOrServiceId   :> Post '[JSON] ThentosSessionToken
  :<|> ReqBody '[JSON] ThentosSessionToken :> Get '[JSON] Bool
  :<|> ReqBody '[JSON] ThentosSessionToken :> Delete '[JSON] ()

thentosThentosSession :: ServerT ThentosThentosSession (Action Void ())
thentosThentosSession =
       startThentosSession
  :<|> existsThentosSession
  :<|> endThentosSession
  where startThentosSession (ByUser (id', pass)) = startThentosSessionByUserId id' pass
        startThentosSession (ByService (id', key)) = startThentosSessionByServiceId id' key


-- * service session

type ThentosServiceSession =
       ReqBody '[JSON] ServiceSessionToken :> Get '[JSON] Bool
  :<|> ReqBody '[JSON] ServiceSessionToken :> "meta" :> Get '[JSON] ServiceSessionMetadata
  :<|> ReqBody '[JSON] ServiceSessionToken :> Delete '[JSON] ()

thentosServiceSession :: ServerT ThentosServiceSession (Action Void ())
thentosServiceSession =
       existsServiceSession
  :<|> getServiceSessionMetadata
  :<|> endServiceSession


-- * servant docs

instance HasDocExtras (RestDocs Api) where
    getCabalVersion _ = Paths.version
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
