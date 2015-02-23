{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE ScopedTypeVariables                      #-}

{-# OPTIONS  #-}

module Thentos.Api
  ( -- * Overview
    -- $overview

    -- * Authorization
    -- $authorization

    ActionStateGlobal
  , ActionState
  , Action
  , runAction', runAction
  , catchAction, logActionError
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
import Control.Monad.Trans.Either (EitherT(EitherT), left, eitherT, runEitherT)
import Control.Monad.Trans.Reader (ReaderT(ReaderT), ask, runReaderT)
import Crypto.Random (CPRG, cprgGenerate)
import Data.Acid (AcidState, QueryEvent, UpdateEvent, EventState, EventResult)
import Data.Acid.Advanced (update', query')
import Data.Maybe (fromMaybe)
import Data.String.Conversions (SBS, ST, cs)
import Data.Thyme.Time ()
import Data.Thyme (UTCTime, getCurrentTime)
import System.Log (Priority(DEBUG))

import qualified Codec.Binary.Base64 as Base64

import System.Log.Missing (logger)
import Thentos.Config
import Thentos.DB
import Thentos.Types
import Thentos.Util


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

catchAction :: Action r a -> (DbError -> Action r a) -> Action r a
catchAction action handler =
    ReaderT $ \ state -> do
        EitherT $ do
            outcome <- runEitherT $ action `runReaderT` state
            case outcome of
                Left e -> runEitherT $ (handler e) `runReaderT` state
                Right v -> return $ Right v

logActionError :: Action r a -> Action r a
logActionError action = action `catchAction` \ e -> do
    logger DEBUG $ "error: " ++ show e
    lift $ left e


-- * actions and acid-state

updateAction :: forall event a r .
                 ( UpdateEvent event
                 , EventState event ~ DB
                 , EventResult event ~ Either DbError a
                 ) => (ThentosClearance -> event) -> Action r a
updateAction = accessAction Nothing update'

queryAction :: forall event a r .
                 ( QueryEvent event
                 , EventState event ~ DB
                 , EventResult event ~ Either DbError a
                 ) => (ThentosClearance -> event) -> Action r a
queryAction = accessAction Nothing query'

-- | Pull a snapshot from the database, pass it to the clearance
-- function in the reader monad, pass the resulting clearance to the
-- uncleared event, and pass the cleared event to either query' or
-- update' (whichever is passed as first arg).
--
-- Optional first argument @overrideClearance@ can be used to do write
-- actions that do something the clearance level from the application
-- would not allow.  Use with care!
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
                 ) => Maybe ThentosClearance
                   -> (AcidState (EventState event) -> event -> Action r (EventResult event))
                   -> (ThentosClearance -> event) -> Action r a
accessAction overrideClearance access unclearedEvent = do
    ((st, _, _), clearanceAbs) <- ask
    now <- TimeStamp <$> liftIO getCurrentTime
    clearanceE :: Either DbError ThentosClearance
        <- (>>= (`clearanceAbs` now)) <$> query' st (SnapShot allowEverything)

    case clearanceE of
        Left err -> lift $ left err
        Right clearance -> do
            result <- access st (unclearedEvent $ fromMaybe clearance overrideClearance)
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

addUnconfirmedUser :: CPRG r => UserFormData -> Action (MVar r) (UserId, ConfirmationToken)
addUnconfirmedUser userData = do
    tok <- freshConfirmationToken
    user <- makeUserFromFormData userData
    updateAction $ AddUnconfirmedUser tok user


-- ** services

addService :: CPRG r => Action (MVar r) (ServiceId, ServiceKey)
addService = do
    sid <- freshServiceId
    key <- freshServiceKey
    hashedKey <- hashServiceKey key
    updateAction $ AddService sid hashedKey
    return (sid, key)


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


-- $overview
--
-- Thentos distinguishes between /transactions/ and /actions/.
--
-- /transactions/ are acidic in the sense used in acid-state, and live
-- in 'ThentosQuery' or 'ThentosUpdate', which are monad transformer
-- stacks over the corresponding acid-state types.  On top of
-- acid-state access, thentos transactions provide error handling and
-- authorization management.
--
-- /actions/ can be composed of more than one acidic transaction in a
-- non-acidic fashion, and possibly do other things involving
-- randomness or the system time.  Actions live in the 'Action' monad
-- transformer stack and provide access to acid-state as well as 'IO',
-- and authentication management.  'queryAction' and 'updateAction'
-- can be used to translate transactions into actions.
--
-- A collection of basic transactions is implemented in "Thentos.DB.Trans",
-- and one of simple actions is implemented in "Thentos.Api".  Software using
-- Thentos as a library is expected to add more transactions and
-- actions and place them in other modules.


-- $authorization
--
-- Access to acid-state data is protected by `DCLabel`s from the lio
-- package.  Each transaction is called in the context of a clearance
-- level ('ThentosClearance') that describes the privileges of the
-- calling user.  Each transaction returns its result together with a
-- label ('ThentosLabel').  The label must satisfy @label `canFlowTo`
-- clearance@ (see 'canFlowTo').  The functions 'accessAction',
-- 'updateAction', 'queryAction' take the clearance level from the
-- application state and attach it to the acid-state event.
--
-- Labels can be combined to a least upper bound (method 'lub' of type
-- class 'Label').  If several transactions are combined to a new,
-- more complex transaction, the latter must be labeled with the least
-- upper bound of the more primitive ones.
--
-- In order to avoid having to check label against clearance
-- explicitly inside every transaction, we use a Thentos-specific
-- derivative of 'makeAcidic' that calls 'Thentos.DB.Core.runThentosUpdate' or
-- 'Thentos.DB.Core.runThentosQuery', resp..  (FIXME: not implemented yet!)
--
-- If you need to implement an action that runs with higher clearance
-- than the current user can present, 'accessAction' takes a 'Maybe'
-- argument to override the presented clearance.
--
-- See the (admittedly rather sparse) lio documentation for more
-- details (or ask the thentos maintainers to elaborate on this :-).
