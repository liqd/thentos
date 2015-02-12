{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE ScopedTypeVariables                      #-}

{-# OPTIONS  #-}

module Api
  ( ActionStateGlobal
  , ActionState
  , Action
  , runAction', runAction
  , updateAction
  , queryAction
  , addUnconfirmedUser
  , addService
  , startSession
  , startSessionNow
  , startSessionNowWithTimeout
  , bumpSession
  , isActiveSession
  , isActiveSessionAndBump
  , isLoggedIntoService
  , addServiceLogin
  , dropServiceLogin
  )
where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar (MVar, modifyMVar)
import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT, left, eitherT)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Crypto.Random (CPRG, cprgGenerate)
import Data.Acid (AcidState, QueryEvent, UpdateEvent, EventState, EventResult)
import Data.Acid.Advanced (update', query')
import Data.String.Conversions (SBS, ST, cs)
import Data.Thyme.Time ()
import Data.Thyme (UTCTime, getCurrentTime)

import qualified Codec.Binary.Base64 as Base64

import Config
import DB
import Types
import Util


-- * types

type ActionStateGlobal r = (AcidState DB, r, ThentosConfig)
type ActionState r = (ActionStateGlobal r, DB -> TimeStamp -> Either DbError ThentosClearance)
type Action r = ReaderT (ActionState r) (EitherT DbError IO)


-- * running actions

runAction :: (MonadIO m, CPRG r) =>
       ActionState (MVar r) -> Action (MVar r) a -> m (Either DbError a)
runAction actionState action =
    liftIO . eitherT (return . Left) (return . Right) $ action `runReaderT` actionState

runAction' :: (MonadIO m, CPRG r) =>
       (ActionStateGlobal (MVar r), ThentosClearance) -> Action (MVar r) a -> m (Either DbError a)
runAction' (asg, clearance) = runAction (asg, \ _ _ -> Right clearance)


-- * actions and acid-state

updateAction :: forall event a r .
                 ( UpdateEvent event
                 , EventState event ~ DB
                 , EventResult event ~ Either DbError a
                 ) => (ThentosClearance -> event) -> Action r a
updateAction = accessAction update'

queryAction :: forall event a r .
                 ( QueryEvent event
                 , EventState event ~ DB
                 , EventResult event ~ Either DbError a
                 ) => (ThentosClearance -> event) -> Action r a
queryAction = accessAction query'

-- | Pull a snapshot from the database, pass it to the clearance
-- function in the reader monad, pass the resulting clearance to the
-- uncleared event, and pass the cleared event to either query' or
-- update' (whichever is passed as first arg).
--
-- NOTE: Authentication check and transaction do *not* form an atomic
-- transaction.  In order to get an upper bound on how long changes in
-- access priviledges need to become effective, the following may work
-- (but is not implemented): 'SnapShot' returns 'DB' together with a
-- timestamp.  When the actual transaction is executed, 'mkAuth' will
-- be passed another timestamp (of the time of the execution of the
-- actual transaction), and can compare the two.  If the snapshot is
-- too old, it can throw an error.  (This would mean that 'mkAuth'
-- would have to live in 'Action', but I don't see any issues with
-- that.)
accessAction :: forall event a r .
                 ( EventState event ~ DB
                 , EventResult event ~ Either DbError a
                 ) => (AcidState (EventState event) -> event -> Action r (EventResult event))
                   -> (ThentosClearance -> event) -> Action r a
accessAction access unclearedEvent = do
    ((st, _, _), clearanceAbs) <- ask
    now <- TimeStamp <$> liftIO getCurrentTime
    clearanceE :: Either DbError ThentosClearance
        <- (>>= (`clearanceAbs` now)) <$> query' st (SnapShot allowEverything)

    case clearanceE of
        Left err -> lift $ left err
        Right clearance -> do
            result <- access st (unclearedEvent clearance)
            case result of
                Left err -> lift $ left err
                Right success -> return success


-- * randomness

-- | A relative of 'cprgGenerate' from crypto-random who lives in
-- 'Action'.
genRandomBytes :: CPRG r => Int -> Action (MVar r) SBS
genRandomBytes i = do
    ((_, mr, _), _) <- ask
    liftIO . modifyMVar mr $ \ r -> do
        let (result, r') = cprgGenerate i r
        return (r', result)


-- | Return a base64 encoded random string of length 24 (18 bytes of
-- entropy).
freshRandomName :: CPRG r => Action (MVar r) ST
freshRandomName = cs . Base64.encode <$> genRandomBytes 18


freshServiceId :: CPRG r => Action (MVar r) ServiceId
freshServiceId = ServiceId <$> freshRandomName

freshServiceKey :: CPRG r => Action (MVar r) ServiceKey
freshServiceKey = ServiceKey <$> freshRandomName

freshSessionToken :: CPRG r => Action (MVar r) SessionToken
freshSessionToken = SessionToken <$> freshRandomName

freshConfirmationToken :: CPRG r => Action (MVar r) ConfirmationToken
freshConfirmationToken = ConfirmationToken <$> freshRandomName


-- ** users

addUnconfirmedUser :: CPRG r => UserFormData -> Action (MVar r) ConfirmationToken
addUnconfirmedUser userData = do
    tok <- freshConfirmationToken
    user <- makeUserFromFormData userData
    updateAction $ AddUnconfirmedUser tok user


-- ** services

addService :: CPRG r => Action (MVar r) (ServiceId, ServiceKey)
addService = do
    sid <- freshServiceId
    key <- freshServiceKey
    updateAction $ AddService sid key


-- ** sessions

startSession :: CPRG r => Agent -> TimeStamp -> Timeout -> Action (MVar r) SessionToken
startSession agent start lifetime = do
    tok <- freshSessionToken
    updateAction $ StartSession tok agent start lifetime

-- | Sessions have a fixed duration of 2 weeks.
startSessionNow :: CPRG r => Agent -> Action (MVar r) SessionToken
startSessionNow agent = startSessionNowWithTimeout (agent, Timeout $ 14 * 24 * 3600)

-- | Sessions with explicit timeout.
startSessionNowWithTimeout :: CPRG r => (Agent, Timeout) -> Action (MVar r) SessionToken
startSessionNowWithTimeout (agent, timeout) = do
    now :: UTCTime <- liftIO getCurrentTime
    startSession agent (TimeStamp now) timeout

bumpSession :: SessionToken -> Action r (SessionToken, Session)
bumpSession tok = do
    now <- TimeStamp <$> liftIO getCurrentTime
    updateAction $ LookupSession (Just (now, True)) tok

isActiveSession :: SessionToken -> Action r Bool
isActiveSession tok = do
    now <- TimeStamp <$> liftIO getCurrentTime
    queryAction $ IsActiveSession now tok

isActiveSessionAndBump :: SessionToken -> Action r Bool
isActiveSessionAndBump tok = do
    now <- TimeStamp <$> liftIO getCurrentTime
    updateAction $ IsActiveSessionAndBump now tok

isLoggedIntoService :: SessionToken -> ServiceId -> Action r Bool
isLoggedIntoService tok sid = do
    now <- TimeStamp <$> liftIO getCurrentTime
    updateAction $ IsLoggedIntoService now tok sid

sessionAndUserIdFromToken :: SessionToken -> Action r (Session, UserId)
sessionAndUserIdFromToken tok = do
    (_, session) <- bumpSession tok
    case session ^. sessionAgent of
        UserA uid -> return (session, uid)
        ServiceA _ -> lift $ left OperationNotPossibleInServiceSession

addServiceLogin :: SessionToken -> ServiceId -> Action r ()
addServiceLogin tok sid = do
    (_, uid) <- sessionAndUserIdFromToken tok
    updateAction $ UpdateUserField uid (UpdateUserFieldAddService sid)

dropServiceLogin :: SessionToken -> ServiceId -> Action r ()
dropServiceLogin tok sid = do
    (_, uid) <- sessionAndUserIdFromToken tok
    updateAction $ UpdateUserField uid (UpdateUserFieldDropService sid)
