{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}

{-# OPTIONS  #-}

module Thentos.Api
  ( -- * Overview
    -- $overview

    -- * Authorization
    -- $authorization

    ActionStateGlobal
  , ActionState
  , Action

  , BadCredentials(..)
  , OperationNotPossibleInServiceSession(..)
  , MalformedConfirmationToken(..)

  , runAction', runAction
  , catchAction, logActionError
  , updateAction
  , queryAction
  , addUnconfirmedUser
  , addPasswordResetToken
  , getUserClearance
  , resetPassword
  , checkPassword
  , addService
  , startSessionUser
  , startSessionService
  , startSessionNoPass
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
import Control.Exception (Exception)
import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT(EitherT), left, eitherT, runEitherT)
import Control.Monad.Trans.Reader (ReaderT(ReaderT), ask, runReaderT)
import Crypto.Random (CPRG, cprgGenerate)
import Data.Acid (AcidState, QueryEvent, UpdateEvent, EventState, EventResult)
import Data.Acid.Advanced (update', query')
import Data.Maybe (fromMaybe)
import Data.SafeCopy (deriveSafeCopy, base)
import Data.String.Conversions (SBS, ST, cs)
import Data.Thyme (getCurrentTime)
import Data.Thyme.Time ()
import Data.Typeable (Typeable)
import System.Log (Priority(DEBUG))

import qualified Codec.Binary.Base64 as Base64

import System.Log.Missing (logger)
import Thentos.Config
import Thentos.DB
import Thentos.Types
import Thentos.Util


-- * types

type ActionStateGlobal r = (AcidState DB, r, ThentosConfig)
type ActionState r = (ActionStateGlobal r, DB -> TimeStamp -> Either SomeThentosError ThentosClearance)
type Action r = ReaderT (ActionState r) (EitherT SomeThentosError IO)


data BadCredentials = BadCredentials
  deriving (Eq, Ord, Show, Read, Typeable)

data OperationNotPossibleInServiceSession = OperationNotPossibleInServiceSession
  deriving (Eq, Ord, Show, Read, Typeable)

data MalformedConfirmationToken = MalformedConfirmationToken ST
  deriving (Eq, Ord, Show, Read, Typeable)

instance ThentosError BadCredentials
instance Exception BadCredentials
$(deriveSafeCopy 0 'base ''BadCredentials)

instance ThentosError OperationNotPossibleInServiceSession
instance Exception OperationNotPossibleInServiceSession
$(deriveSafeCopy 0 'base ''OperationNotPossibleInServiceSession)

instance ThentosError MalformedConfirmationToken
instance Exception MalformedConfirmationToken
$(deriveSafeCopy 0 'base ''MalformedConfirmationToken)


-- * running actions

runAction :: (MonadIO m, CPRG r, e ~ SomeThentosError) =>
       ActionState (MVar r) -> Action (MVar r) a -> m (Either e a)
runAction actionState action =
    liftIO . eitherT (return . Left) (return . Right) $ action `runReaderT` actionState

runAction' :: (MonadIO m, CPRG r, e ~ SomeThentosError) =>
       (ActionStateGlobal (MVar r), ThentosClearance) -> Action (MVar r) a -> m (Either e a)
runAction' (asg, clearance) = runAction (asg, \ _ _ -> Right clearance)

catchAction :: forall e r a . ThentosError e => Action r a -> (e -> Action r a) -> Action r a
catchAction action handler =
    ReaderT $ \ state -> do
        EitherT $ do
            outcome <- runEitherT $ action `runReaderT` state
            case outcome of
                Left me -> case fromThentosError me :: Maybe e of
                    Just e -> runEitherT $ (handler e) `runReaderT` state
                    Nothing -> return $ Left me
                Right v -> return $ Right v

logActionError :: Action r a -> Action r a
logActionError action = action `catchAction` \ (e :: SomeThentosError) -> do
    logger DEBUG $ "error: " ++ show e
    lift $ left e


-- * actions and acid-state

updateAction :: forall event e a r .
                 ( e ~ SomeThentosError
                 , UpdateEvent event
                 , EventState event ~ DB
                 , EventResult event ~ Either e a
                 ) => (ThentosClearance -> event) -> Action r a
updateAction = accessAction Nothing update'

queryAction :: forall event e a r .
                 ( e ~ SomeThentosError
                 , QueryEvent event
                 , EventState event ~ DB
                 , EventResult event ~ Either e a
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
accessAction :: forall event e a r .
                 ( e ~ SomeThentosError
                 , EventState event ~ DB
                 , EventResult event ~ Either e a
                 ) => Maybe ThentosClearance
                   -> (AcidState (EventState event) -> event -> Action r (EventResult event))
                   -> (ThentosClearance -> event) -> Action r a
accessAction overrideClearance access unclearedEvent = do
    ((st, _, _), clearanceAbs) <- ask
    now <- TimeStamp <$> liftIO getCurrentTime
    clearanceE :: Either SomeThentosError ThentosClearance
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

freshPasswordResetToken :: CPRG r => Action (MVar r) PasswordResetToken
freshPasswordResetToken = PasswordResetToken <$> freshRandomName


-- ** users

addUnconfirmedUser :: CPRG r => UserFormData -> Action (MVar r) (UserId, ConfirmationToken)
addUnconfirmedUser userData = do
    tok <- freshConfirmationToken
    user <- makeUserFromFormData userData
    updateAction $ AddUnconfirmedUser tok user

addPasswordResetToken :: CPRG r => UserEmail -> Action (MVar r) (User, PasswordResetToken)
addPasswordResetToken email = do
    now <- TimeStamp <$> liftIO getCurrentTime
    tok <- freshPasswordResetToken
    user <- updateAction $ AddPasswordResetToken now email tok
    return (user, tok)

resetPassword :: PasswordResetToken -> UserPass -> Action r ()
resetPassword token password = do
    now <- TimeStamp <$> liftIO getCurrentTime
    hashedPassword <- hashUserPass password
    updateAction $ ResetPassword now token hashedPassword

checkPassword :: UserName -> UserPass -> Action r (Maybe (UserId, User))
checkPassword username password = do
    catchAction checkPw $ \ (_ :: NoSuchUser) -> return Nothing
  where
    checkPw = do
        (uid, user) <- queryAction $ LookupUserByName username
        return $ if verifyPass password user
            then Just (uid, user)
            else Nothing

getUserClearance :: UserId -> Action r ThentosClearance
getUserClearance uid = do
    roles <- queryAction $ LookupAgentRoles (UserA uid)
    return $ makeClearance (UserA uid) roles


-- ** services

addService :: CPRG r => ServiceName -> ServiceDescription -> Action (MVar r) (ServiceId, ServiceKey)
addService name desc = do
    sid <- freshServiceId
    key <- freshServiceKey
    hashedKey <- hashServiceKey key
    updateAction $ AddService sid hashedKey name desc
    return (sid, key)


-- ** sessions

-- | Check user credentials and create a session for user.
startSessionUser :: CPRG r => (UserId, UserPass) -> Action (MVar r) SessionToken
startSessionUser (uid, pass) = do
    (_, user) <- accessAction (Just allowEverything) query' $ LookupUser uid
    if verifyPass pass user
        then startSessionNoPass (UserA uid)
        else lift . left . toThentosError $ BadCredentials

-- | Check service credentials and create a session for service.
startSessionService :: CPRG r => (ServiceId, ServiceKey) -> Action (MVar r) SessionToken
startSessionService (sid, key) = do
    (_, service) <- accessAction (Just allowEverything) query' $ LookupService sid
    if verifyKey key service
        then startSessionNoPass (ServiceA sid)
        else lift . left . toThentosError $ BadCredentials

-- | Do NOT check credentials, and just open a session for any agent.
-- Only call this in the context of a successful authentication check!
startSessionNoPass :: CPRG r => Agent -> Action (MVar r) SessionToken
startSessionNoPass agent = do
    now <- TimeStamp <$> liftIO getCurrentTime
    tok <- freshSessionToken
    accessAction (Just allowEverything) update' $ StartSession tok agent now defaultSessionTimeout

-- | Sessions have a fixed duration of 2 weeks.
defaultSessionTimeout :: Timeout
defaultSessionTimeout = Timeout $ 14 * 24 * 3600

bumpSession :: SessionToken -> Action r (SessionToken, Session)
bumpSession tok = do
    now <- TimeStamp <$> liftIO getCurrentTime
    updateAction $ LookupSession (Just (now, True)) tok

isActiveSession :: SessionToken -> Action r Bool
isActiveSession tok = do
    now <- TimeStamp <$> liftIO getCurrentTime
    queryAction $ IsActiveSession now tok

-- | FUTURE WORK [performance]: do a query first; if session has not
-- aged beyond a certain threshold, do not bump.  (also, we should
-- just bump the session in 'makeThentosClearance' and then be done
-- with it.  much simpler!)
isActiveSessionAndBump :: SessionToken -> Action r Bool
isActiveSessionAndBump tok = do
    now <- TimeStamp <$> liftIO getCurrentTime
    updateAction $ IsActiveSessionAndBump now tok

isLoggedIntoService :: SessionToken -> ServiceId -> Action r Bool
isLoggedIntoService tok sid = do
    now <- TimeStamp <$> liftIO getCurrentTime
    updateAction $ IsLoggedIntoService now tok sid

_sessionAndUserIdFromToken :: SessionToken -> Action r (Session, UserId)
_sessionAndUserIdFromToken tok = do
    (_, session) <- bumpSession tok
    case session ^. sessionAgent of
        UserA uid -> return (session, uid)
        ServiceA _ -> lift . left . toThentosError $ OperationNotPossibleInServiceSession

addServiceLogin :: SessionToken -> ServiceId -> Action r ()
addServiceLogin tok sid = do
    (_, uid) <- _sessionAndUserIdFromToken tok
    updateAction $ UpdateUserField uid (UpdateUserFieldAddService sid)

dropServiceLogin :: SessionToken -> ServiceId -> Action r ()
dropServiceLogin tok sid = do
    (_, uid) <- _sessionAndUserIdFromToken tok
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
