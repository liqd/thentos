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

module Backend.Api.Simple (runApi, serveApi, apiDocs) where

import Control.Concurrent.MVar (MVar)
import Crypto.Random (SystemRNG)
import Data.Acid (AcidState)
import Data.CaseInsensitive (CI)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (SBS, ST)
import Data.Text.Encoding (decodeUtf8')
import Network.Wai (Application, requestHeaders)
import Network.Wai.Handler.Warp (run)
import Servant.API ((:<|>)((:<|>)), (:>), Get, Post, Put, Delete, Capture, ReqBody)
import Servant.Docs (HasDocs, docsFor, docs, markdown)
import Servant.Server.Internal (HasServer, Server, route)
import Servant.Server (serve)

import Api
import Backend.Core
import DB
import Doc ()
import Types


runApi :: Int -> (AcidState DB, MVar SystemRNG) -> IO ()
runApi port = run port . serveApi

-- | (Required in test suite.)
serveApi :: (AcidState DB, MVar SystemRNG) -> Application
serveApi = serve (Proxy :: Proxy App) . app

apiDocs :: String
apiDocs = markdown $ docs (Proxy :: Proxy App)


-- * the application

type App = ThentosAuth ThentosBasic

app :: (AcidState DB, MVar SystemRNG) -> Server App
app (st, rng) = ThentosAuth (st, rng) thentosBasic

type ThentosBasic =
       "user" :> ThentosUser
  :<|> "service" :> ThentosService
  :<|> "session" :> ThentosSession

thentosBasic :: PushActionSubRoute (Server ThentosBasic)
thentosBasic =
       thentosUser
  :<|> thentosService
  :<|> thentosSession


-- * user

type ThentosUser =
       Get [UserId]
  :<|> Capture "userid" UserId :> Get (UserId, User)
  :<|> ReqBody User :> Post UserId
  :<|> Capture "userid" UserId :> ReqBody User :> Put ()
  :<|> Capture "userid" UserId :> Delete

thentosUser :: PushActionSubRoute (Server ThentosUser)
thentosUser =
       queryAction AllUserIds
  :<|> queryAction . LookupUser
  :<|> updateAction . AddUser
  :<|> (\ uid user -> updateAction $ UpdateUser uid user)
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
       createSession
  :<|> createSessionWithTimeout
  :<|> updateAction . EndSession
  :<|> isActiveSession


-- * authentication

-- | Empty data type for triggering authentication.  If you have an
-- api type 'API', use like this: @ThentosAuth :> API@, then write a
-- route handler that takes 'Auth' as an extra argument.  'Auth' will
-- be parsed from the headers and injected into the @sublayout@
-- handler.
data ThentosAuth layout = ThentosAuth (AcidState DB, MVar SystemRNG) layout

instance ( PushActionC (Server sublayout)
         , HasServer sublayout
         ) => HasServer (ThentosAuth sublayout)
  where
    type Server (ThentosAuth sublayout) = ThentosAuth (PushActionSubRoute (Server sublayout))

    route Proxy (ThentosAuth (st, rng) subserver) request respond =
        route (Proxy :: Proxy sublayout) (pushAction routingState subserver) request respond
      where
        pluck :: CI SBS -> Maybe ST
        pluck key = lookup key (requestHeaders request) >>= either (const Nothing) Just . decodeUtf8'

        routingState :: RestActionState
        routingState = ( st
                       , \ db -> mkThentosClearance
                           (pluck "X-Thentos-User")
                           (pluck "X-Thentos-Service")
                           (pluck "X-Thentos-Password")
                           db
                       , rng)

-- | FIXME: not much documentation yet.
instance HasDocs sublayout => HasDocs (ThentosAuth sublayout) where
  docsFor Proxy = docsFor (Proxy :: Proxy sublayout)
