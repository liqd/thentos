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

{-# OPTIONS  #-}

module Thentos.Backend.Api.Simple (App, ThentosAuth, runBackend, serveApi) where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar (MVar)
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Crypto.Random (SystemRNG)
import Data.Proxy (Proxy(Proxy))
import Network.Wai (Application)
import Servant.API ((:<|>)((:<|>)), (:>), Get, Post, Put, Delete, Capture, ReqBody)
import Servant.Server.Internal (HasServer, Server, route)
import Servant.Server (serve)
import System.Log.Logger (Priority(INFO))

import System.Log.Missing (logger)
import Thentos.Api
import Thentos.Backend.Api.Proxy
import Thentos.Backend.Core (RestActionState, PushActionC, PushActionSubRoute, pushAction)
import Thentos.Backend.Core (ThentosAssertHeaders, ThentosHeaderName(..), lookupRequestHeader, runWarpWithCfg)
import Thentos.Config
import Thentos.DB
import Thentos.Types
import Thentos.Util


runBackend :: HttpConfig -> ActionStateGlobal (MVar SystemRNG) -> IO ()
runBackend cfg asg = do
    logger INFO $ "running rest api (simple style) on " ++show (bindUrl cfg) ++ "."
    runWarpWithCfg cfg $ serveApi asg

-- | (Required in test suite.)
serveApi :: ActionStateGlobal (MVar SystemRNG) -> Application
serveApi = serve (Proxy :: Proxy App) . app


-- * the application

type App = ThentosAssertHeaders (ThentosAuth ThentosBasic)

app :: ActionStateGlobal (MVar SystemRNG) -> Server App
app asg = ThentosAuth asg thentosBasic

type ThentosBasic =
       "user" :> ThentosUser
  :<|> "service" :> ThentosService
  :<|> "session" :> ThentosSession
  :<|> "proxy-test" :> ServiceProxy

thentosBasic :: PushActionSubRoute (Server ThentosBasic)
thentosBasic =
       thentosUser
  :<|> thentosService
  :<|> thentosSession
  :<|> serviceProxy


-- * authentication

-- | Empty data type for triggering authentication.  If you have an
-- api type 'API', use like this: @ThentosAuth :> API@, then write a
-- route handler that takes 'Auth' as an extra argument.  'Auth' will
-- be parsed from the headers and injected into the @sublayout@
-- handler.
data ThentosAuth layout = ThentosAuth (ActionStateGlobal (MVar SystemRNG)) layout

instance ( PushActionC (Server sublayout)
         , HasServer sublayout
         ) => HasServer (ThentosAuth sublayout)
  where
    type Server (ThentosAuth sublayout) = ThentosAuth (PushActionSubRoute (Server sublayout))

    route Proxy (ThentosAuth asg subserver) request respond =
        route (Proxy :: Proxy sublayout) (pushAction routingState subserver) request respond
      where
        routingState :: RestActionState
        routingState = ( asg
                       , makeThentosClearance $ lookupRequestHeader request ThentosHeaderSession
                       )


-- * user

type ThentosUser =
       ReqBody UserFormData :> Post UserId
  :<|> Capture "uid" UserId :> Delete
  :<|> Capture "uid" UserId :> "name" :> ReqBody UserName :> Put ()
  :<|> Capture "uid" UserId :> "name" :> Get UserName
  :<|> Capture "uid" UserId :> "email" :> ReqBody UserEmail :> Put ()
  :<|> Capture "uid" UserId :> "email" :> Get UserEmail
  :<|> Get [UserId]


thentosUser :: PushActionSubRoute (Server ThentosUser)
thentosUser =
       (\ userFormData -> liftIO (makeUserFromFormData userFormData) >>= updateAction . AddUser)
  :<|> updateAction . DeleteUser
  :<|> (\ uid name -> updateAction $ UpdateUserField uid (UpdateUserFieldName name))
  :<|> (((^. userName) . snd) <$>) . queryAction . LookupUser
  :<|> (\ uid email -> updateAction $ UpdateUserField uid (UpdateUserFieldEmail email))
  :<|> (((^. userEmail) . snd) <$>) . queryAction . LookupUser
  :<|> queryAction AllUserIds


-- * service

type ThentosService =
       ReqBody (ServiceName, ServiceDescription) :> Post (ServiceId, ServiceKey)
  :<|> Capture "sid" ServiceId :> Delete
  :<|> Get [ServiceId]

thentosService :: PushActionSubRoute (Server ThentosService)
thentosService =
         uncurry addService
    :<|> updateAction . DeleteService
    :<|> queryAction AllServiceIds


-- * session

type ThentosSession =
       ReqBody (UserId, UserPass)      :> Post SessionToken
  :<|> ReqBody (ServiceId, ServiceKey) :> Post SessionToken
  :<|> ReqBody SessionToken            :> Get Bool
  :<|> ReqBody SessionToken            :> Delete
  :<|> ReqBody SessionToken            :> Capture "sid" ServiceId :> Post ()
  :<|> ReqBody SessionToken            :> Capture "sid" ServiceId :> Get Bool
  :<|> ReqBody SessionToken            :> Capture "sid" ServiceId :> Delete

thentosSession :: PushActionSubRoute (Server ThentosSession)
thentosSession =
       startSessionUser
  :<|> startSessionService
  :<|> isActiveSession
  :<|> updateAction . EndSession
  :<|> addServiceLogin
  :<|> isLoggedIntoService
  :<|> dropServiceLogin
