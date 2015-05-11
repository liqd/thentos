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
{-# LANGUAGE PackageImports                           #-}

module Thentos.Backend.Api.Simple (App, ThentosAuth, runBackend, serveApi) where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar (MVar)
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import "crypto-random" Crypto.Random (SystemRNG)
import Data.Proxy (Proxy(Proxy))
import Network.Wai (Application)
import Servant.API ((:<|>)((:<|>)), (:>), Get, Post, Put, Delete, Capture, ReqBody)
import Servant.Server.Internal (HasServer, Server, route)
import Servant.Server (serve)
import System.Log.Logger (Priority(INFO))

import System.Log.Missing (logger)
import Thentos.Action
import Thentos.Action.Core  -- FIXME: this shouldn't be here.  use only things from Thentos.Action!
import Thentos.Backend.Api.Proxy
import Thentos.Backend.Core (PushActionC, PushActionSubRoute, pushAction)
import Thentos.Backend.Core (ThentosAssertHeaders, ThentosHeaderName(..), lookupRequestHeader, runWarpWithCfg)
import Thentos.Config
import Thentos.Types
import Thentos.Util

import qualified Thentos.Transaction as T  -- FIXME: this shouldn't be here.  use Thentos.Action instead!


runBackend :: HttpConfig -> ActionState -> IO ()
runBackend cfg asg = do
    logger INFO $ "running rest api (simple style) on " ++ show (bindUrl cfg) ++ "."
    runWarpWithCfg cfg $ serveApi asg

-- | (Required in test suite.)
serveApi :: ActionState -> Application
serveApi = serve (Proxy :: Proxy App) . app


-- * the application

type App = ThentosAssertHeaders (ThentosAuth ThentosBasic)

app :: ActionState -> Server App
app asg = ThentosAuth asg thentosBasic

type ThentosBasic =
       "user" :> ThentosUser
  :<|> "service" :> ThentosService
  :<|> "session" :> ThentosSessionApi  -- FIXME: call all these types '...Api'
  :<|> "servicesession" :> ThentosServiceSession
  :<|> "proxy-test" :> ServiceProxy

thentosBasic :: PushActionSubRoute (Server ThentosBasic)
thentosBasic =
       thentosUser
  :<|> thentosService
  :<|> thentosSession
  :<|> thentosServiceSession
  :<|> serviceProxy


-- * authentication

-- | Empty data type for triggering authentication.  If you have an
-- api type 'API', use like this: @ThentosAuth :> API@, then write a
-- route handler that takes 'Auth' as an extra argument.  'Auth' will
-- be parsed from the headers and injected into the @sublayout@
-- handler.
data ThentosAuth layout = ThentosAuth ActionState layout

instance ( PushActionC (Server sublayout)
         , HasServer sublayout
         ) => HasServer (ThentosAuth sublayout)
  where
    type Server (ThentosAuth sublayout) = ThentosAuth (PushActionSubRoute (Server sublayout))

    route Proxy (ThentosAuth asg subserver) request respond =
        route (Proxy :: Proxy sublayout) (pushAction routingState subserver) request respond
      where
        routingState :: ActionState
        routingState = asg


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
       (>>= update'P . T.AddUser) . makeUserFromFormData'P
  :<|> update'P . T.DeleteUser
  :<|> (\ uid name -> update'P $ T.UpdateUserField uid (T.UpdateUserFieldName name))
  :<|> (((^. userName) . snd) <$>) . query'P . T.LookupUser
  :<|> (\ uid email -> update'P $ T.UpdateUserField uid (T.UpdateUserFieldEmail email))
  :<|> (((^. userEmail) . snd) <$>) . query'P . T.LookupUser
  :<|> query'P T.AllUserIds


-- * service

type ThentosService =
       ReqBody (UserId, ServiceName, ServiceDescription) :> Post (ServiceId, ServiceKey)
           -- FIXME: it would be much nicer to infer the owner from
           -- the session token, but that requires changes to the
           -- various action monads we are kicking around all over the
           -- place.  coming up soon!

  :<|> Capture "sid" ServiceId :> Delete
  :<|> Get [ServiceId]

thentosService :: PushActionSubRoute (Server ThentosService)
thentosService =
         (\ (uid, sn, sd) -> addService (UserA uid) sn sd)
    :<|> update'P . T.DeleteService
    :<|> query'P T.AllServiceIds


-- * session

type ThentosSessionApi =
       ReqBody (UserId, UserPass)      :> Post ThentosSessionToken
  :<|> ReqBody (ServiceId, ServiceKey) :> Post ThentosSessionToken
  :<|> ReqBody ThentosSessionToken     :> Get Bool
  :<|> ReqBody ThentosSessionToken     :> Delete

thentosSession :: PushActionSubRoute (Server ThentosSessionApi)
thentosSession =
       uncurry startThentosSessionByUserId
  :<|> uncurry startThentosSessionByServiceId
  :<|> existsThentosSession
  :<|> endThentosSession


-- * service session
type ThentosServiceSession =
       Capture "token" ServiceSessionToken :> Delete
  :<|> Capture "token" ServiceSessionToken :> "meta" :> Get ServiceSessionMetadata
  :<|> Capture "token" ServiceSessionToken :> Get Bool

thentosServiceSession :: PushActionSubRoute (Server ThentosServiceSession)
thentosServiceSession =
       endServiceSession
  :<|> getServiceSessionMetadata
  :<|> existsServiceSession
