{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE DataKinds                                #-}
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
  , confirmNewUser
  , addPasswordResetToken
  , getUserClearance
  , changePassword
  , resetPassword
  , checkPasswordByUserName
  , checkPasswordByUserId
  , requestUserEmailChange
  , confirmUserEmailChange
  , addService
  , userGroups
  , startThentosSessionByUserId
  , startThentosSessionByUserName
  , startSessionService
  , startSessionNoPass
  , bumpSession
  , isActiveSession
  , isActiveSessionAndBump
  , isActiveServiceSession
  , getSessionServiceNames
  , addServiceRegistration
  , dropServiceRegistration
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
import Data.Configifier ((>>.), Tagged(Tagged))
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(Proxy))
import Data.Set (Set)
import Data.String.Conversions (SBS, ST, cs)
import Data.Thyme (getCurrentTime)
import System.Log (Priority(DEBUG))

import qualified Codec.Binary.Base64 as Base64
import qualified Data.Map as Map
import qualified Data.Set as Set

import System.Log.Missing (logger)
import Thentos.Config
import Thentos.DB
import Thentos.Smtp (sendEmailChangeConfirmationMail)
import Thentos.Types
import Thentos.Util


-- * types

type ActionStateGlobal r = (AcidState DB, r, ThentosConfig)
type ActionState r = (ActionStateGlobal r, DB -> Timestamp -> Either ThentosError ThentosClearance)
type Action r = ReaderT (ActionState r) (EitherT ThentosError IO)


-- * running actions

runAction :: (MonadIO m, CPRG r) =>
       ActionState (MVar r) -> Action (MVar r) a -> m (Either ThentosError a)
runAction actionState action =
    liftIO . eitherT (return . Left) (return . Right) $ action `runReaderT` actionState

runAction' :: (MonadIO m, CPRG r) =>
       (ActionStateGlobal (MVar r), ThentosClearance) -> Action (MVar r) a -> m (Either ThentosError a)
runAction' (asg, clearance) = runAction (asg, \ _ _ -> Right clearance)

catchAction :: Action r a -> (ThentosError -> Action r a) -> Action r a
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
                 , EventResult event ~ Either ThentosError a
                 ) => (ThentosClearance -> event) -> Action r a
updateAction = accessAction Nothing update'

queryAction :: forall event a r .
                 ( QueryEvent event
                 , EventState event ~ DB
                 , EventResult event ~ Either ThentosError a
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
-- access privileges need to become effective, the following may work
-- (but is not implemented): 'SnapShot' returns 'DB' together with a
-- timestamp.  When the actual transaction is executed, 'mkAuth' will
-- be passed another timestamp (of the time of the execution of the
-- actual transaction), and can compare the two.  If the snapshot is
-- too old, it can throw an error.  (This would mean that 'mkAuth'
-- would have to live in 'Action', but I don't see any issues with
-- that.)
accessAction :: forall event a r .
                 ( EventState event ~ DB
                 , EventResult event ~ Either ThentosError a
                 ) => Maybe ThentosClearance
                   -> (AcidState (EventState event) -> event -> Action r (EventResult event))
                   -> (ThentosClearance -> event) -> Action r a
accessAction overrideClearance access unclearedEvent = do
    ((st, _, _), clearanceAbs) <- ask
    now <- Timestamp <$> liftIO getCurrentTime
    clearanceE :: Either ThentosError ThentosClearance
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

freshServiceSessionToken :: CPRG r => Action (MVar r) ServiceSessionToken
freshServiceSessionToken = ServiceSessionToken <$> freshRandomName


-- ** users

addUnconfirmedUser :: CPRG r => UserFormData -> Action (MVar r) (UserId, ConfirmationToken)
addUnconfirmedUser userData = do
    now <- Timestamp <$> liftIO getCurrentTime
    tok <- freshConfirmationToken
    user <- makeUserFromFormData userData
    updateAction $ AddUnconfirmedUser now tok user

confirmNewUser :: ConfirmationToken -> Action (MVar r) UserId
confirmNewUser token = do
    ((_, _, config), _) <- ask
    let expiryPeriod = config >>. (Proxy :: Proxy '["user_reg_expiration"])
    now <- Timestamp <$> liftIO getCurrentTime
    updateAction $ FinishUserRegistration now expiryPeriod token

addPasswordResetToken :: CPRG r => UserEmail -> Action (MVar r) (User, PasswordResetToken)
addPasswordResetToken email = do
    now <- Timestamp <$> liftIO getCurrentTime
    tok <- freshPasswordResetToken
    user <- updateAction $ AddPasswordResetToken now email tok
    return (user, tok)

resetPassword :: PasswordResetToken -> UserPass -> Action r ()
resetPassword token password = do
    now <- Timestamp <$> liftIO getCurrentTime
    ((_, _, config), _) <- ask
    let expiryPeriod = config >>. (Proxy :: Proxy '["pw_reset_expiration"])
    hashedPassword <- hashUserPass password
    updateAction $ ResetPassword now expiryPeriod token hashedPassword

changePassword :: UserId -> UserPass -> UserPass -> Action r ()
changePassword uid old new = do
    _ <- checkPasswordByUserId uid old
    hashedPw <- hashUserPass new
    updateAction $ UpdateUserField uid (UpdateUserFieldPassword hashedPw)

checkPasswordByUserName :: UserName -> UserPass -> Action r (UserId, User)
checkPasswordByUserName username password =
    checkPassword (LookupUserByName username) password

checkPasswordByUserId :: UserId -> UserPass -> Action r (UserId, User)
checkPasswordByUserId uid password = checkPassword (LookupUser uid) password

checkPassword :: (QueryEvent event,
                  EventResult event
                    ~ Either ThentosError (UserId, User),
                  EventState event ~ DB) =>
     (ThentosClearance -> event) -> UserPass -> Action r (UserId, User)
checkPassword action password = catchAction checkPw $ \err ->
    case err of
        NoSuchUser -> lift $ left BadCredentials
        e -> lift $ left e
  where
    checkPw = do
        (uid, user) <- accessAction (Just allowEverything) query' action  -- FIXME: label action, don't override clearance.
        if verifyPass password user
            then return (uid, user)
            else lift $ left BadCredentials

requestUserEmailChange :: CPRG r =>
    UserId -> UserEmail -> (ConfirmationToken -> ST) -> Action (MVar r) ()
requestUserEmailChange uid newEmail callbackUrlBuilder = do
    tok <- freshConfirmationToken
    ((_, _, config), _) <- ask
    now <- Timestamp <$> liftIO getCurrentTime
    updateAction $ AddUserEmailChangeRequest now uid newEmail tok
    let callbackUrl = callbackUrlBuilder tok
        smtpConfig = Tagged $ config >>. (Proxy :: Proxy '["smtp"])
    liftIO $ sendEmailChangeConfirmationMail smtpConfig newEmail callbackUrl
    return ()

-- | Look up the given confirmation token and updates the user's email address
-- iff 1) the token exists and 2) the token belongs to the user and 3) the token
-- has not expired. If any of these conditions don't apply, throw
-- 'NoSuchToken' to avoid leaking information.
confirmUserEmailChange :: ConfirmationToken -> Action (MVar r) ()
confirmUserEmailChange token = do
    now <- Timestamp <$> liftIO getCurrentTime
    ((_, _, config), _) <- ask
    let expiryPeriod = config >>. (Proxy :: Proxy '["email_change_expiration"])
    catchAction
        (updateAction $ ConfirmUserEmailChange now expiryPeriod token)
        $ \err -> case err of
            PermissionDenied _ _ _ -> lift (left NoSuchToken)
            e                      -> lift (left e)

getUserClearance :: UserId -> Action r ThentosClearance
getUserClearance uid = do
    roles <- queryAction $ LookupAgentRoles (UserA uid)
    return $ makeClearance (UserA uid) roles


-- ** services

addService :: CPRG r => Agent -> ServiceName -> ServiceDescription -> Action (MVar r) (ServiceId, ServiceKey)
addService owner name desc = do
    sid <- freshServiceId
    key <- freshServiceKey
    hashedKey <- hashServiceKey key
    updateAction $ AddService owner sid hashedKey name desc
    return (sid, key)

-- | List all group leafs a user is member in on some service.
userGroups :: UserId -> ServiceId -> Action r [Group]
userGroups uid sid = do
    (_, service) <- queryAction $ LookupService sid

    let groupMap :: Map.Map GroupNode (Set Group)
        groupMap = service ^. serviceGroups

        memberships :: GroupNode -> Set Group
        memberships g = Map.findWithDefault Set.empty g groupMap

        unionz :: Set (Set Group) -> Set Group
        unionz = Set.fold Set.union Set.empty

        f :: GroupNode -> Set Group
        f g@(GroupU _) =                r g
        f g@(GroupG n) = n `Set.insert` r g

        r :: GroupNode -> Set Group
        r g = unionz $ Set.map (f . GroupG) (memberships g)

    return . Set.toList . f . GroupU $ uid


-- ** sessions

-- | Check user credentials and create a session for user.
startThentosSessionByUserId :: CPRG r => UserId -> UserPass -> Action (MVar r) SessionToken
startThentosSessionByUserId uid pass = do
    _ <- checkPasswordByUserId uid pass
    startSessionNoPass (UserA uid)

startThentosSessionByUserName :: CPRG r => UserName-> UserPass -> Action (MVar r) (UserId, SessionToken)
startThentosSessionByUserName name pass = do
    (uid, _) <- checkPasswordByUserName name pass
    token <- startSessionNoPass (UserA uid)
    return (uid, token)

-- | Check service credentials and create a session for service.
startSessionService :: CPRG r => ServiceId -> ServiceKey -> Action (MVar r) SessionToken
startSessionService sid key = do
    (_, service) <- accessAction (Just allowEverything) query' $ LookupService sid
    if verifyKey key service
        then startSessionNoPass (ServiceA sid)
        else lift $ left BadCredentials

-- | Do NOT check credentials, and just open a session for any agent.
-- Only call this in the context of a successful authentication check!
startSessionNoPass :: CPRG r => Agent -> Action (MVar r) SessionToken
startSessionNoPass agent = do
    now <- Timestamp <$> liftIO getCurrentTime
    tok <- freshSessionToken
    accessAction (Just allowEverything) update' $ StartSession tok agent now defaultSessionTimeout
    return tok

-- | Sessions have a fixed duration of 2 weeks.
--
-- FIXME: this is used for both service sessions and thentos sessions.
-- delete this name, and make two new names that disentangle that.
--
-- FIXME: make configurable.  (eventually, this will need to be
-- run-time configurable.  but there will probably still be global
-- defaults handled by configifier.)
defaultSessionTimeout :: Timeout
defaultSessionTimeout = Timeout $ 14 * 24 * 3600

bumpSession :: SessionToken -> Action r (SessionToken, Session)
bumpSession tok = do
    now <- Timestamp <$> liftIO getCurrentTime
    updateAction $ LookupSession (Just (now, True)) tok

isActiveSession :: SessionToken -> Action r Bool
isActiveSession tok = do
    now <- Timestamp <$> liftIO getCurrentTime
    queryAction $ IsActiveSession now tok

-- | FUTURE WORK [performance]: do a query first; if session has not
-- aged beyond a certain threshold, do not bump.  (also, we should
-- just bump the session in 'makeThentosClearance' and then be done
-- with it.  much simpler!)
isActiveSessionAndBump :: SessionToken -> Action r Bool
isActiveSessionAndBump tok = do
    now <- Timestamp <$> liftIO getCurrentTime
    updateAction $ IsActiveSessionAndBump now tok

isActiveServiceSession :: ServiceSessionToken -> Action r Bool
isActiveServiceSession tok = do
    now <- Timestamp <$> liftIO getCurrentTime
    updateAction $ IsActiveServiceSession now tok

getSessionServiceNames :: SessionToken -> UserId -> Action r [ServiceName]
getSessionServiceNames sid uid = do
    now <- Timestamp <$> liftIO getCurrentTime
    queryAction $ GetSessionServiceNames now sid uid

_sessionAndUserIdFromToken :: SessionToken -> Action r (Session, UserId)
_sessionAndUserIdFromToken tok = do
    (_, session) <- bumpSession tok
    case session ^. sessionAgent of
        UserA uid -> return (session, uid)
        ServiceA _ -> lift $ left OperationNotPossibleInServiceSession

addServiceRegistration :: SessionToken -> ServiceId -> Action r ()
addServiceRegistration tok sid = do
    (_, uid) <- _sessionAndUserIdFromToken tok
    updateAction $ UpdateUserField uid (UpdateUserFieldAddService sid newServiceAccount)

dropServiceRegistration :: SessionToken -> ServiceId -> Action r ()
dropServiceRegistration tok sid = do
    (_, uid) <- _sessionAndUserIdFromToken tok
    updateAction $ UpdateUserField uid (UpdateUserFieldDropService sid)

-- | If user is not registered, throw an error.
addServiceLogin :: CPRG r => SessionToken -> ServiceId -> Action (MVar r) ServiceSessionToken
addServiceLogin tok sid = do
    now <- Timestamp <$> liftIO getCurrentTime
    serviceSessionToken <- freshServiceSessionToken
    updateAction $ AddServiceLogin now defaultSessionTimeout tok sid serviceSessionToken

-- | If user is not registered, throw an error.
dropServiceLogin :: ServiceSessionToken -> Action r ()
dropServiceLogin tok = updateAction $ DropServiceLogin tok


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
