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

import Control.Applicative ((<$>))
import Control.Concurrent.MVar (MVar)
import Crypto.Random (SystemRNG)
import Control.Monad.State (liftIO)
import Data.Acid (AcidState)
import Data.CaseInsensitive (CI)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (SBS, ST)
import Data.Text.Encoding (decodeUtf8')
import Data.Thyme.Time ()
import Data.Thyme (UTCTime, getCurrentTime)
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
       getUserIds
  :<|> getUser
  :<|> postNewUser
  :<|> postNamedUser
  :<|> deleteUser

getUserIds :: RestAction [UserId]
getUserIds = queryAction AllUserIds

getUser :: UserId -> RestAction (UserId, User)
getUser = queryAction . LookupUser

postNewUser :: User -> RestAction UserId
postNewUser = updateAction . AddUser

postNamedUser :: UserId -> User -> RestAction ()
postNamedUser uid = updateAction . UpdateUser uid

deleteUser :: UserId -> RestAction ()
deleteUser = updateAction . DeleteUser


-- * service

type ThentosService =
       Get [ServiceId]
  :<|> Capture "sid" ServiceId :> Get (ServiceId, Service)
  :<|> Post (ServiceId, ServiceKey)

thentosService :: PushActionSubRoute (Server ThentosService)
thentosService =
         getServiceIds
    :<|> getService
    :<|> postNewService

getServiceIds :: RestAction [ServiceId]
getServiceIds = queryAction AllServiceIds

getService :: ServiceId -> RestAction (ServiceId, Service)
getService = queryAction . LookupService

postNewService :: RestAction (ServiceId, ServiceKey)
postNewService = addService


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
  :<|> endSession
  :<|> isActiveSession

-- | Sessions have a fixed duration of 2 weeks.
createSession :: (UserId, ServiceId) -> RestAction SessionToken
createSession (uid, sid) = createSessionWithTimeout (uid, sid, Timeout $ 14 * 24 * 3600)

-- | Sessions with explicit timeout.
createSessionWithTimeout :: (UserId, ServiceId, Timeout) -> RestAction SessionToken
createSessionWithTimeout (uid, sid, timeout) = do
    now :: UTCTime <- liftIO getCurrentTime
    startSession uid sid (TimeStamp now) timeout

endSession :: SessionToken -> RestAction ()
endSession = updateAction . EndSession

isActiveSession :: SessionToken -> RestAction Bool
isActiveSession tok = do
    now <- TimeStamp <$> liftIO getCurrentTime
    queryAction $ IsActiveSession now tok


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
