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

module Thentos.Backend.Api.Simple where

import Control.Applicative ((<$>))
import Control.Lens ((^.))
import Data.Proxy (Proxy(Proxy))
import Network.Wai (Application)
import Servant.API ((:<|>)((:<|>)), (:>), Get, Post, Put, Delete, Capture, ReqBody, JSON)
import Servant.Server (ServerT, Server, serve, enter)
import System.Log.Logger (Priority(INFO))

import System.Log.Missing (logger)
import Thentos.Action
import Thentos.Action.Core (ActionState, Action)
import Thentos.Backend.Api.Auth
import Thentos.Backend.Core
import Thentos.Config
import Thentos.Types


-- * main

runApi :: HttpConfig -> ActionState DB -> IO ()
runApi cfg asg = do
    logger INFO $ "running rest api Thentos.Backend.Api.Simple on " ++ show (bindUrl cfg) ++ "."
    runWarpWithCfg cfg $ serveApi asg

serveApi :: ActionState DB -> Application
serveApi = addCacheControlHeaders . serve (Proxy :: Proxy Api) . api

type Api = ThentosAssertHeaders :> ThentosAuth :> ThentosBasic

api :: ActionState DB -> Server Api
api actionState mTok = enter (enterAction actionState mTok) thentosBasic


-- * combinators

type ThentosBasic =
       "user" :> ThentosUser
  :<|> "service" :> ThentosService
  :<|> "thentos_session" :> ThentosThentosSession
  :<|> "service_session" :> ThentosServiceSession

thentosBasic :: ServerT ThentosBasic (Action DB)
thentosBasic =
       thentosUser
  :<|> thentosService
  :<|> thentosThentosSession
  :<|> thentosServiceSession


-- * user

type ThentosUser =
       ReqBody '[JSON] UserFormData :> Post '[JSON] UserId
  :<|> Capture "uid" UserId :> Delete '[JSON] ()
  :<|> Capture "uid" UserId :> "name" :> ReqBody '[JSON] UserName :> Put '[JSON] ()
  :<|> Capture "uid" UserId :> "name" :> Get '[JSON] UserName
  :<|> Capture "uid" UserId :> "email" :> ReqBody '[JSON] UserEmail :> Put '[JSON] ()
  :<|> Capture "uid" UserId :> "email" :> Get '[JSON] UserEmail
  :<|> Get '[JSON] [UserId]

thentosUser :: ServerT ThentosUser (Action DB)
thentosUser =
       addUser
  :<|> deleteUser
  :<|> (\ uid name -> updateUserField uid (UpdateUserFieldName name))
  :<|> (((^. userName) . snd) <$>) . lookupUser
  :<|> (\ uid email -> updateUserField uid (UpdateUserFieldEmail email))
  :<|> (((^. userEmail) . snd) <$>) . lookupUser
  :<|> allUserIds


-- * service

type ThentosService =
       ReqBody '[JSON] (UserId, ServiceName, ServiceDescription) :> Post '[JSON] (ServiceId, ServiceKey)
           -- FIXME: it would be much nicer to infer the owner from
           -- the session token, but that requires changes to the
           -- various action monads we are kicking around all over the
           -- place.  coming up soon!

  :<|> Capture "sid" ServiceId :> Delete '[JSON] ()
  :<|> Get '[JSON] [ServiceId]

thentosService :: ServerT ThentosService (Action DB)
thentosService =
         (\ (uid, sn, sd) -> addService (UserA uid) sn sd)
    :<|> deleteService
    :<|> allServiceIds


-- * session

type ThentosThentosSession =
       ReqBody '[JSON] (UserId, UserPass)      :> Post '[JSON] ThentosSessionToken
  :<|> ReqBody '[JSON] (ServiceId, ServiceKey) :> Post '[JSON] ThentosSessionToken
  :<|> ReqBody '[JSON] ThentosSessionToken     :> Get '[JSON] Bool
  :<|> ReqBody '[JSON] ThentosSessionToken     :> Delete '[JSON] ()

thentosThentosSession :: ServerT ThentosThentosSession (Action DB)
thentosThentosSession =
       uncurry startThentosSessionByUserId
  :<|> uncurry startThentosSessionByServiceId
  :<|> existsThentosSession
  :<|> endThentosSession


-- * service session

type ThentosServiceSession =
       ReqBody '[JSON] ServiceSessionToken :> Get '[JSON] Bool
  :<|> ReqBody '[JSON] ServiceSessionToken :> "meta" :> Get '[JSON] ServiceSessionMetadata
  :<|> ReqBody '[JSON] ServiceSessionToken :> Delete '[JSON] ()

thentosServiceSession :: ServerT ThentosServiceSession (Action DB)
thentosServiceSession =
       existsServiceSession
  :<|> getServiceSessionMetadata
  :<|> endServiceSession
