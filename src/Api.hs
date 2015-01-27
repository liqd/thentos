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

module Api (runApi, serveApi, apiDocs) where

import Control.Applicative ((<$>))
import Control.Monad.State (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT, left)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Data.Acid (AcidState, QueryEvent, UpdateEvent, EventState, EventResult)
import Data.Acid.Advanced (update', query')
import Data.AffineSpace ((.+^))
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

import DB
import Types
import Doc ()

runApi :: Int -> AcidState DB -> IO ()
runApi port = run port . serveApi

-- | (Required in test suite.)
serveApi :: AcidState DB -> Application
serveApi = serve (Proxy :: Proxy App) . app

apiDocs :: String
apiDocs = markdown $ docs (Proxy :: Proxy App)


-- * the application

type App = ThentosAuth ThentosBasic

app :: AcidState DB -> Server App
app st = ThentosAuth st thentosBasic

type ThentosBasic =
       "user" :> ThentosUser
  :<|> "service" :> ThentosService
  :<|> "session" :> ThentosSession

thentosBasic :: PushReaderSubType (Server ThentosBasic)
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

thentosUser :: PushReaderSubType (Server ThentosUser)
thentosUser =
       getUserIds
  :<|> getUser
  :<|> postNewUser
  :<|> postNamedUser
  :<|> deleteUser

getUserIds :: RestActionLabeled [UserId]
getUserIds = queryServant AllUserIDs

getUser :: UserId -> RestActionLabeled (UserId, User)
getUser = queryServant . LookupUser

postNewUser :: User -> RestActionLabeled UserId
postNewUser = updateServant . AddUser

postNamedUser :: UserId -> User -> RestActionLabeled ()
postNamedUser uid = updateServant . UpdateUser uid

deleteUser :: UserId -> RestActionLabeled ()
deleteUser = updateServant . DeleteUser


-- * service

type ThentosService =
       Get [ServiceId]
  :<|> Capture "sid" ServiceId :> Get (ServiceId, Service)
  :<|> Post ServiceId

thentosService :: PushReaderSubType (Server ThentosService)
thentosService =
         getServiceIds
    :<|> getService
    :<|> postNewService

getServiceIds :: RestActionLabeled [ServiceId]
getServiceIds = queryServant AllServiceIDs

getService :: ServiceId -> RestActionLabeled (ServiceId, Service)
getService = queryServant . LookupService

postNewService :: RestActionLabeled ServiceId
postNewService = updateServant AddService


-- * session

type ThentosSession =
       Get [SessionToken]
  :<|> Capture "token" SessionToken :> Get (SessionToken, Session)
  :<|> ReqBody (UserId, ServiceId) :> Post SessionToken
  :<|> ReqBody (UserId, ServiceId, Timeout) :> Post SessionToken
  :<|> Capture "token" SessionToken :> "logout" :> Get ()
  :<|> Capture "sid" ServiceId :> Capture "token" SessionToken :> "active" :> Get Bool

thentosSession :: PushReaderSubType (Server ThentosSession)
thentosSession =
       getSessionTokens
  :<|> getSession
  :<|> createSession
  :<|> createSessionWithTimeout
  :<|> endSession
  :<|> isActiveSession

getSessionTokens :: RestActionLabeled [SessionToken]
getSessionTokens = queryServant AllSessionTokens

getSession :: SessionToken -> RestActionLabeled (SessionToken, Session)
getSession = queryServant . LookupSession

-- | Sessions have a fixed duration of 2 weeks.
createSession :: (UserId, ServiceId) -> RestActionLabeled SessionToken
createSession (uid, sid) = createSessionWithTimeout (uid, sid, Timeout $ 14 * 24 * 3600)

-- | Sessions with explicit timeout.
createSessionWithTimeout :: (UserId, ServiceId, Timeout) -> RestActionLabeled SessionToken
createSessionWithTimeout (uid, sid, Timeout diff) = do
    now :: UTCTime <- liftIO getCurrentTime
    updateServant $ StartSession uid sid (TimeStamp now) (TimeStamp $ now .+^ diff)

endSession :: SessionToken -> RestActionLabeled ()
endSession = updateServant . EndSession

isActiveSession :: ServiceId -> SessionToken -> RestActionLabeled Bool
isActiveSession sid = queryServant . IsActiveSession sid


-- * authentication

-- | this is a work-around: The Server type family always terminates
-- in 'RestActionRaw' on all methods.  'PushReaderT' instances
-- transform handlers implemented in a monad stack that contains the
-- acid state and authentication information in a reader into the
-- handlers that we need.
class PushReaderT a where
    type PushReaderSubType a
    unPushReaderT :: RoutingState -> PushReaderSubType a -> a

instance (PushReaderT b) => PushReaderT (a -> b) where
    type PushReaderSubType (a -> b) = a -> PushReaderSubType b
    unPushReaderT clearance f = unPushReaderT clearance . f

instance (PushReaderT a, PushReaderT b) => PushReaderT (a :<|> b) where
    type PushReaderSubType (a :<|> b) = PushReaderSubType a :<|> PushReaderSubType b
    unPushReaderT clearance (a :<|> b) = unPushReaderT clearance a :<|> unPushReaderT clearance b

instance PushReaderT (RestActionRaw a) where
    type PushReaderSubType (RestActionRaw a) = RestActionLabeled a
    unPushReaderT rstate x = runReaderT x rstate


-- | Empty data type for triggering authentication.  If you have an
-- api type 'API', use like this: @ThentosAuth :> API@, then write a
-- route handler that takes 'Auth' as an extra argument.  'Auth' will
-- be parsed from the headers and injected into the @sublayout@
-- handler.
data ThentosAuth layout = ThentosAuth (AcidState DB) layout

instance ( PushReaderT (Server sublayout)
         , HasServer sublayout
         ) => HasServer (ThentosAuth sublayout)
  where
    type Server (ThentosAuth sublayout) = ThentosAuth (PushReaderSubType (Server sublayout))

    route Proxy (ThentosAuth st subserver) request respond =
        route (Proxy :: Proxy sublayout) (unPushReaderT routingState subserver) request respond
      where
        pluck :: CI SBS -> Maybe ST
        pluck key = lookup key (requestHeaders request) >>= either (const Nothing) Just . decodeUtf8'

        routingState :: RoutingState
        routingState = ( st
                       , \ db -> mkThentosClearance
                           (pluck "X-Thentos-User")
                           (pluck "X-Thentos-Service")
                           (pluck "X-Thentos-Password")
                           db)

-- | FIXME: not much documentation yet.
instance HasDocs sublayout => HasDocs (ThentosAuth sublayout) where
  docsFor Proxy = docsFor (Proxy :: Proxy sublayout)


-- * plumbing

type RoutingState = (AcidState DB, DB -> Either DbError ThentosClearance)
type RestActionRaw = EitherT (Int, String) IO
type RestActionLabeled = ReaderT RoutingState RestActionRaw

updateServant :: forall event a .
                 ( UpdateEvent event
                 , EventState event ~ DB
                 , EventResult event ~ Either DbError a
                 ) => (ThentosClearance -> event) -> RestActionLabeled a
updateServant = accessServant update'

queryServant :: forall event a .
                 ( QueryEvent event
                 , EventState event ~ DB
                 , EventResult event ~ Either DbError a
                 ) => (ThentosClearance -> event) -> RestActionLabeled a
queryServant = accessServant query'

-- | Pull a snapshot from the database, pass it to the clearance
-- function in the reader monad, pass the resulting clearance to the
-- uncleared event, and pass the cleared event to either query' or
-- update' (whichever is passed as first arg).
--
-- NOTE: Authentication check and transaction do *not* form an atomic
-- transaction.  In order to get an upper bound on how long changes in
-- access priviledges need to become effective, the following may work
-- (but is not implemented): 'SnapShot' returns 'DB' together with a
-- timestamp.  When the actual transaction is executed, mkAuth will be
-- passed another timestamp (of the time of the execution of the
-- actual transaction), and can compare the two.  If the 'DB' snapshot
-- is too old, it can throw an error.  (This would mean that mkAuth
-- would have to live in 'RestActionLabeled', but I don't see any
-- issues with that.)  Anyway this is a little over-complicated for
-- now, I think.
accessServant :: forall event a .
                 ( EventState event ~ DB
                 , EventResult event ~ Either DbError a
                 ) => (AcidState (EventState event) -> event -> RestActionLabeled (EventResult event))
                   -> (ThentosClearance -> event) -> RestActionLabeled a
accessServant access unclearedEvent = do
    (st, clearanceAbs) <- ask
    clearanceE :: Either DbError ThentosClearance <- (>>= clearanceAbs) <$> query' st (SnapShot thentosCleared)
    case clearanceE of
        Left err -> lift . left . showDbError $ err
        Right clearance -> do
            result <- access st (unclearedEvent clearance)
            case result of
                Left err -> lift . left . showDbError $ err
                Right success -> return success
