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

module Backend.Api.Simple (runBackend, serveApi, apiDocs) where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar (MVar)
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Crypto.Random (SystemRNG)
import Data.Proxy (Proxy(Proxy))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant.API ((:<|>)((:<|>)), (:>), Get, Post, Put, Delete, Capture, ReqBody)
import Servant.Docs (HasDocs, docsFor, docs, markdown)
import Servant.Server.Internal (HasServer, Server, route)
import Servant.Server (serve)

import Api
import Backend.Api.Proxy
import Backend.Core (RestActionState, PushActionC, PushActionSubRoute, pushAction, lookupRequestHeader)
import DB
import Doc ()
import Types
import Util


runBackend :: Int -> ActionStateGlobal (MVar SystemRNG) -> IO ()
runBackend port = run port . serveApi

-- | (Required in test suite.)
serveApi :: ActionStateGlobal (MVar SystemRNG) -> Application
serveApi = serve (Proxy :: Proxy App) . app

apiDocs :: String
apiDocs = markdown $ docs (Proxy :: Proxy App)


-- * the application

type App = ThentosAuth ThentosBasic

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
                       , makeThentosClearance
                           (lookupRequestHeader request "X-Thentos-User")
                           (lookupRequestHeader request "X-Thentos-Service")
                           (lookupRequestHeader request "X-Thentos-Password")
                           (lookupRequestHeader request "X-Thentos-Session")
                       )

-- | FIXME: not much documentation yet.
instance HasDocs sublayout => HasDocs (ThentosAuth sublayout) where
  docsFor Proxy = docsFor (Proxy :: Proxy sublayout)


-- * user

type ThentosUser =
       Get [UserId]
  :<|> Capture "userid" UserId :> "name" :> Get UserName
  :<|> Capture "userid" UserId :> "name" :> ReqBody UserName :> Put ()
  :<|> Capture "userid" UserId :> "email" :> Get UserEmail
  :<|> Capture "userid" UserId :> "email" :> ReqBody UserEmail :> Put ()
  :<|> ReqBody UserFormData :> Post UserId
  :<|> Capture "userid" UserId :> Delete


thentosUser :: PushActionSubRoute (Server ThentosUser)
thentosUser =
       queryAction AllUserIds
  :<|> (((^. userName) . snd) <$>) . queryAction . LookupUser
  :<|> (\ uid name -> updateAction $ UpdateUserField uid (UpdateUserFieldName name))
  :<|> (((^. userEmail) . snd) <$>) . queryAction . LookupUser
  :<|> (\ uid email -> updateAction $ UpdateUserField uid (UpdateUserFieldEmail email))
  :<|> (\ userFormData -> liftIO (makeUserFromFormData userFormData) >>= updateAction . AddUser)
  :<|> updateAction . DeleteUser


-- * service

type ThentosService =
       Get [ServiceId]
  :<|> Capture "sid" ServiceId :> Get (ServiceId, Service)
  :<|> Post (ServiceId, ServiceKey)

thentosService :: PushActionSubRoute (Server ThentosService)
thentosService =
         queryAction AllServiceIds
    :<|> queryAction . LookupService
    :<|> addService


-- * session

type ThentosSession =
       ReqBody (UserId, ServiceId) :> Post SessionToken
  :<|> ReqBody (UserId, ServiceId, Timeout) :> Post SessionToken
  :<|> Capture "token" SessionToken :> Delete
  :<|> Capture "token" SessionToken :> Get Bool

thentosSession :: PushActionSubRoute (Server ThentosSession)
thentosSession =
       startSessionNow
  :<|> startSessionNowWithTimeout
  :<|> updateAction . EndSession
  :<|> isActiveSession
