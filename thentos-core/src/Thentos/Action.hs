{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE ViewPatterns         #-}

module Thentos.Action
    ( freshRandomName
    , freshConfirmationToken
    , freshPasswordResetToken
    , freshServiceId
    , freshServiceKey
    , freshSessionToken
    , freshServiceSessionToken
    , freshSsoToken

    , allUserIds
    , lookupUser
    , lookupUserByName
    , lookupUserByEmail
    , addUser
    , addUsers
    , deleteUser
    , assertUserIsNew
    , addUnconfirmedUser
    , addUnconfirmedUserWithId
    , confirmNewUser
    , addPasswordResetToken
    , resetPassword
    , changePassword
    , requestUserEmailChange
    , confirmUserEmailChange
    , updateUserField, T.UpdateUserFieldOp(..)
    , updateUserFields

    , allServiceIds
    , lookupService
    , addService
    , deleteService
    , userGroups

    , defaultSessionTimeout
    , lookupThentosSession
    , existsThentosSession
    , startThentosSessionByUserId
    , startThentosSessionByUserName
    , startThentosSessionByUserEmail
    , startThentosSessionByServiceId
    , endThentosSession
    , serviceNamesFromThentosSession

    , lookupServiceSession
    , existsServiceSession
    , addServiceRegistration
    , dropServiceRegistration
    , startServiceSession
    , endServiceSession
    , getServiceSessionMetadata

    , assignRole
    , unassignRole
    , agentRoles

    , addNewSsoToken
    , lookupAndRemoveSsoToken

    , collectGarbage
    )
where

import Control.Applicative ((<$>), (<*>))
import Control.Lens ((^.))
import Control.Monad (unless, void)
import Control.Monad.Except (throwError, catchError)
import Data.Acid (QueryEvent, EventState, EventResult)
import Data.Configifier ((>>.), Tagged(Tagged))
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (ST, cs)
import GHC.Exception (Exception)
import LIO.Core (liftLIO, guardWrite, taint)
import LIO.DCLabel ((%%), (\/), (/\))
import LIO.Error (AnyLabelError)

import qualified Codec.Binary.Base64 as Base64
import qualified Data.Set as Set
import qualified Data.Text as ST

import LIO.Missing
import Thentos.Action.Core
import Thentos.Types
import Thentos.Util

import qualified Thentos.Transaction as T


-- * randomness

-- | Return a base64 encoded random string of length 24 (18 bytes of entropy).
-- We use @_@ instead of @/@ as last letter of the base64 alphabet since it allows using names
-- within URLs without percent-encoding. Our Base64 alphabet thus consists of ASCII letters +
-- digits as well as @+@ and @_@. All of these are reliably recognized in URLs, even if they occur
-- at the end.
--
-- RFC 4648 also has a "URL Safe Alphabet" which additionally replaces @+@ by @-@. But that's
-- problematic, since @-@ at the end of URLs is not recognized as part of the URL by some programs
-- such as Thunderbird.
freshRandomName :: Action db ST
freshRandomName = ST.replace "/" "_" . cs . Base64.encode <$> genRandomBytes'P 18

freshConfirmationToken :: Action db ConfirmationToken
freshConfirmationToken = ConfirmationToken <$> freshRandomName

freshPasswordResetToken :: Action db PasswordResetToken
freshPasswordResetToken = PasswordResetToken <$> freshRandomName

freshServiceId :: Action db ServiceId
freshServiceId = ServiceId <$> freshRandomName

freshServiceKey :: Action db ServiceKey
freshServiceKey = ServiceKey <$> freshRandomName

freshSessionToken :: Action db ThentosSessionToken
freshSessionToken = ThentosSessionToken <$> freshRandomName

freshServiceSessionToken :: Action db ServiceSessionToken
freshServiceSessionToken = ServiceSessionToken <$> freshRandomName

freshSsoToken :: Action db SsoToken
freshSsoToken = SsoToken <$> freshRandomName


-- * user

-- | Return a list of all 'UserId's.  Requires 'RoleAdmin'.
allUserIds :: (db `Extends` DB) => Action db [UserId]
allUserIds = do
    liftLIO $ taint (RoleAdmin %% False)
    query'P T.AllUserIds

-- | Return a user with its id.  Requires or privileges of admin or the user that is looked up.  If
-- no user is found or access is not granted, throw 'NoSuchUser'.  See '_lookupUserCheckPassword' for
-- user lookup prior to authentication.
lookupUser :: (db `Extends` DB) => UserId -> Action db (UserId, User)
lookupUser uid = _lookupUser $ T.LookupUser uid

_lookupUser :: ( QueryEvent event
               , EventState event ~ db
               , EventResult event ~ Either (ThentosError db) (UserId, User)
               , db `Extends` DB) =>
               event -> Action db (UserId, User)
_lookupUser transaction = do
    val@(uid, _) <- query'P transaction
    tryTaint (RoleAdmin \/ UserA uid %% False)
        (return val)
        (\ (_ :: AnyLabelError) -> throwError $ thentosErrorFromParent NoSuchUser)

-- | Like 'lookupUser', but based on 'UserName'.
lookupUserByName :: (db `Extends` DB) => UserName -> Action db (UserId, User)
lookupUserByName name = _lookupUser $ T.LookupUserByName name

-- | Like 'lookupUser', but based on 'UserEmail'.
lookupUserByEmail :: (db `Extends` DB) => UserEmail -> Action db (UserId, User)
lookupUserByEmail email = _lookupUser $ T.LookupUserByEmail email

-- | Add a user based on its form data.  Requires 'RoleAdmin'.  For creating users with e-mail
-- verification, see 'addUnconfirmedUser', 'confirmNewUser'.
addUser :: (db `Extends` DB, Exception (ActionError db)) => UserFormData -> Action db UserId
addUser userData = do
    liftLIO $ guardWrite (RoleAdmin %% RoleAdmin)
    makeUserFromFormData'P userData >>= update'P . T.AddUser

addUsers :: (db `Extends` DB, Exception (ActionError db)) => [UserFormData] -> Action db [UserId]
addUsers userData = do
    liftLIO $ guardWrite (RoleAdmin %% RoleAdmin)
    users <- mapM makeUserFromFormData'P userData
    update'P $ T.AddUsers users

-- | Delete user.  Requires or privileges of admin or the user that is looked up.  If no user is
-- found or access is not granted, throw 'NoSuchUser'.
deleteUser :: (db `Extends` DB, Exception (ActionError db)) => UserId -> Action db ()
deleteUser uid = do
    liftLIO $ guardWrite (RoleAdmin \/ UserA uid %% RoleAdmin /\ UserA uid)
    update'P $ T.DeleteUser uid

-- | Assert that no user with the same name or email address already exists in the db.
-- Does not require any privileges.
assertUserIsNew :: (db `Extends` DB) => UserFormData -> Action db ()
assertUserIsNew userData = do
    user <- makeUserFromFormData'P userData
    query'P $ T.AssertUserIsNew user


-- ** email confirmation

-- | Initiate email-verified user creation.  Does not require any privileges.  See also:
-- 'confirmNewUser'.
addUnconfirmedUser :: (db `Extends` DB, Exception (ActionError db)) =>
    UserFormData -> Action db (UserId, ConfirmationToken)
addUnconfirmedUser userData = do
    (now, tok, user) <- prepareUserData userData
    update'P $ T.AddUnconfirmedUser now tok user

-- | Initiate email-verified user creation, assigning a specific ID to the new user.
-- If the ID is already in use, an error is thrown. Does not require any privileges.
addUnconfirmedUserWithId :: (db `Extends` DB, Show (ActionError db)) =>
    UserFormData -> UserId -> Action db ConfirmationToken
addUnconfirmedUserWithId userData userId = do
    (now, tok, user) <- prepareUserData userData
    update'P $ T.AddUnconfirmedUserWithId now tok user userId

-- | Collect the data needed for the /addUnconfirmedUser.../ calls.
prepareUserData :: (db `Extends` DB) =>
    UserFormData -> Action db (Timestamp, ConfirmationToken, User)
prepareUserData userData = (,,) <$> getCurrentTime'P <*> freshConfirmationToken
                                <*> makeUserFromFormData'P userData

-- | Finish email-verified user creation.
--
-- SECURITY: As a caller, you have to make sure the token has been produced by the legitimate
-- recipient of a confirmation email.  Authentication can only be provided by this api **after** the
-- user has been created by calling this function.
--
-- See also: 'addUnconfirmedUser'.
confirmNewUser :: (db `Extends` DB, Exception (ActionError db)) =>
    ConfirmationToken -> Action db (UserId, ThentosSessionToken)
confirmNewUser token = do
    expiryPeriod <- (>>. (Proxy :: Proxy '["user_reg_expiration"])) <$> getConfig'P
    now <- getCurrentTime'P
    uid <- update'P $ T.FinishUserRegistration now expiryPeriod token
    sessionToken <- _startThentosSessionByAgent (UserA uid)
    return (uid, sessionToken)


-- ** password reset

-- | Initiate password reset with email confirmation.  No authentication required, obviously.
addPasswordResetToken :: (db `Extends` DB, Exception (ActionError db)) =>
    UserEmail -> Action db (User, PasswordResetToken)
addPasswordResetToken email = do
    now <- getCurrentTime'P
    tok <- freshPasswordResetToken
    user <- update'P $ T.AddPasswordResetToken now email tok
    return (user, tok)

-- | Finish password reset with email confirmation.
--
-- SECURITY: See 'confirmNewUser'.
resetPassword :: (db `Extends` DB, Exception (ActionError db)) =>
    PasswordResetToken -> UserPass -> Action db ()
resetPassword token password = do
    now <- getCurrentTime'P
    expiryPeriod <- (>>. (Proxy :: Proxy '["pw_reset_expiration"])) <$> getConfig'P
    hashedPassword <- hashUserPass'P password
    update'P $ T.ResetPassword now expiryPeriod token hashedPassword


-- ** login

-- | Find user running the action, confirm the password, and return the user or crash.  'NoSuchUser'
-- is translated into 'BadCredentials'.
-- NOTE: This should not be exported from this module, as it allows access to
-- the user map without any clearance.
_lookupUserCheckPassword :: ( QueryEvent event
                            , EventState event ~ db
                            , EventResult event ~ Either (ThentosError db) (UserId, User)
                            , db `Extends` DB
                            ) =>
    event -> UserPass -> Action db (UserId, User)
_lookupUserCheckPassword transaction password = a `catchError` h
  where
    a = do
        (uid, user) <- query'P transaction
        if verifyPass password user
            then return (uid, user)
            else throwError $ thentosErrorFromParent BadCredentials

    h (thentosErrorToParent -> Just NoSuchUser) = throwError $ thentosErrorFromParent BadCredentials
    h e                                         = throwError e


-- ** change user data

-- | FIXME: In combination with 'addServiceRegistration', the label of this function may constitute
-- an integrity breach: If a service does not authorize registration of a user with a service, that
-- user may be able to login without consent of the service, especially once we have anonymous
-- login.  See also 'updateUserFields'.
updateUserField :: (db `Extends` DB, Exception (ActionError db)) =>
    UserId -> T.UpdateUserFieldOp -> Action db ()
updateUserField uid op = do
    liftLIO $ guardWrite (RoleAdmin \/ UserA uid %% RoleAdmin /\ UserA uid)
    update'P $ T.UpdateUserField uid op

-- | See 'updateUserField'.
updateUserFields :: (db `Extends` DB, Exception (ActionError db)) =>
    UserId -> [T.UpdateUserFieldOp] -> Action db ()
updateUserFields uid ops = do
    liftLIO $ guardWrite (RoleAdmin \/ UserA uid %% RoleAdmin /\ UserA uid)
    update'P $ T.UpdateUserFields uid ops

-- | Authenticate user against old password, and then change password to new password.  Requires
-- 'RoleAdmin' or privs of user that owns the password.
changePassword ::
    (db `Extends` DB, Show (ActionError db)) => UserId -> UserPass -> UserPass -> Action db ()
changePassword uid old new = do
    _ <- _lookupUserCheckPassword (T.LookupUser uid) old
    hashedPw <- hashUserPass'P new
    liftLIO $ guardWrite (RoleAdmin \/ UserA uid %% RoleAdmin /\ UserA uid)
    update'P $ T.UpdateUserField uid (T.UpdateUserFieldPassword hashedPw)

-- | Initiate email change by creating and storing a token and sending it out by email to the old
-- address of the user.  This requires 'RoleAdmin' or privs of email address owner, but the address
-- is only changed after a call to 'confirmUserEmailChange' with the correct token.
requestUserEmailChange :: (db `Extends` DB, Exception (ActionError db)) =>
    UserId -> UserEmail -> (ConfirmationToken -> ST) -> Action db ()
requestUserEmailChange uid newEmail callbackUrlBuilder = do
    liftLIO $ guardWrite (RoleAdmin \/ UserA uid %% RoleAdmin /\ UserA uid)

    tok <- freshConfirmationToken
    now <- getCurrentTime'P
    smtpConfig <- (Tagged . (>>. (Proxy :: Proxy '["smtp"]))) <$> getConfig'P

    update'P $ T.AddUserEmailChangeRequest now uid newEmail tok

    let message = "Please go to " <> callbackUrlBuilder tok <> " to confirm your change of email address."
        subject = "Thentos email address change"
    sendMail'P smtpConfig Nothing newEmail subject message

-- | Look up the given confirmation token and updates the user's email address iff 1) the token
-- exists, 2) the token belongs to the user currently logged in, and 3) the token has not
-- expired. If any of these conditions don't apply, throw 'NoSuchToken' to avoid leaking
-- information.
--
-- SECURITY: The security information from 'confirmNewUser' does not directly apply here: the
-- attacker needs to fulfil **all** three conditions mentioned above for a successful attack, not
-- only token secrecy.
confirmUserEmailChange :: ( db `Extends` DB , Exception (ActionError db)) =>
    ConfirmationToken -> Action db ()
confirmUserEmailChange token = do
    now <- getCurrentTime'P
    expiryPeriod <- (>>. (Proxy :: Proxy '["email_change_expiration"])) <$> getConfig'P
    ((uid, _), _) <- query'P $ T.LookupEmailChangeToken token
    tryGuardWrite (RoleAdmin \/ UserA uid %% RoleAdmin /\ UserA uid)
                  (void . update'P $ T.ConfirmUserEmailChange now expiryPeriod token)
                  (\ (_ :: AnyLabelError) -> throwError $ thentosErrorFromParent NoSuchToken)


-- * service

allServiceIds :: (db `Extends` DB, Exception (ActionError db)) => Action db [ServiceId]
allServiceIds = do
    liftLIO $ taint (RoleAdmin %% False)
    query'P T.AllServiceIds

lookupService :: (db `Extends` DB, Exception (ActionError db)) =>
    ServiceId -> Action db (ServiceId, Service)
lookupService sid = do
    liftLIO $ taint (RoleAdmin \/ ServiceA sid %% False)
    query'P $ T.LookupService sid

addService :: (db `Extends` DB, Exception (ActionError db)) =>
    Agent -> ServiceName -> ServiceDescription -> Action db (ServiceId, ServiceKey)
addService owner name desc = do
    liftLIO $ guardWrite (RoleAdmin \/ owner %% RoleAdmin /\ owner)
    sid <- freshServiceId
    key <- freshServiceKey
    hashedKey <- hashServiceKey'P key
    update'P $ T.AddService owner sid hashedKey name desc
    return (sid, key)

deleteService :: (db `Extends` DB, Exception (ActionError db)) => ServiceId -> Action db ()
deleteService sid = do
    liftLIO $ guardWrite (RoleAdmin \/ ServiceA sid %% RoleAdmin /\ ServiceA sid)
    update'P $ T.DeleteService sid

-- | List all group leafs a user is member in on some service.
userGroups :: (db `Extends` DB, Exception (ActionError db)) =>
    UserId -> ServiceId -> Action db [Group]
userGroups uid sid = do
    liftLIO $ taint (UserA uid \/ ServiceA sid %% False)
    (_, service) <- query'P $ T.LookupService sid
    return $ T.flattenGroups service uid


-- * thentos session

-- | Thentos and service sessions have a fixed duration of 2 weeks.
--
-- FIXME: make configurable, and distinguish between thentos sessions and service sessions.
-- (eventually, this will need to be run-time configurable.  but there will probably still be global
-- defaults handled by configifier.)
defaultSessionTimeout :: Timeout
defaultSessionTimeout = Timeout $ 14 * 24 * 3600

-- | Find 'ThentosSession' from token.  If 'ThentosSessionToken' does not exist or clearance does
-- not allow access, throw 'NoSuchThentosSession'.
lookupThentosSession :: (db `Extends` DB, Exception (ActionError db)) =>
    ThentosSessionToken -> Action db ThentosSession
lookupThentosSession tok = do
    now <- getCurrentTime'P
    session <- snd <$> update'P (T.LookupThentosSession now tok)
    tryTaint (session ^. thSessAgent %% False)
        (return session)
        (\ (_ :: AnyLabelError) -> throwError $ thentosErrorFromParent NoSuchThentosSession)

-- | Like 'lookupThentosSession', but does not throw an exception if thentos session does not exist
-- or is inaccessible, but returns 'False' instead.
existsThentosSession :: (db `Extends` DB, Show (ActionError db)) =>
    ThentosSessionToken -> Action db Bool
existsThentosSession tok = (lookupThentosSession tok >> return True) `catchError`
    \case (thentosErrorToParent -> Just NoSuchThentosSession) -> return False
          e                                                   -> throwError e

-- | Check user credentials and create a session for user.  Requires 'lub' or 'lookupUser' and
-- '_startThentosSessionByAgent'.
startThentosSessionByUserId :: (db `Extends` DB, Show (ActionError db)) =>
    UserId -> UserPass -> Action db ThentosSessionToken
startThentosSessionByUserId uid pass = do
    _ <- _lookupUserCheckPassword (T.LookupUser uid) pass
    _startThentosSessionByAgent (UserA uid)

-- | Like 'startThentosSessionByUserId', but based on 'UserName' as key.
startThentosSessionByUserName :: (db `Extends` DB, Show (ActionError db)) =>
    UserName -> UserPass -> Action db (UserId, ThentosSessionToken)
startThentosSessionByUserName name pass = do
    (uid, _) <- _lookupUserCheckPassword (T.LookupUserByName name) pass
    (uid,) <$> _startThentosSessionByAgent (UserA uid)

startThentosSessionByUserEmail :: (db `Extends` DB, Show (ActionError db)) =>
    UserEmail -> UserPass -> Action db (UserId, ThentosSessionToken)
startThentosSessionByUserEmail email pass = do
    (uid, _) <- _lookupUserCheckPassword (T.LookupUserByEmail email) pass
    (uid,) <$> _startThentosSessionByAgent (UserA uid)

-- | Check service credentials and create a session for service.
startThentosSessionByServiceId :: (db `Extends` DB, Show (ActionError db)) =>
    ServiceId -> ServiceKey -> Action db ThentosSessionToken
startThentosSessionByServiceId sid key = a `catchError` h
  where
    a = do
        (_, service) <- query'P (T.LookupService sid)
        unless (verifyKey key service) $ throwError $
            thentosErrorFromParent BadCredentials
        _startThentosSessionByAgent (ServiceA sid)

    h (thentosErrorToParent -> Just NoSuchService) =
        throwError $ thentosErrorFromParent BadCredentials
    h e = throwError e

-- | Terminate 'ThentosSession'.  Does not require any label; being in possession of the session
-- token is enough authentication to terminate it.
endThentosSession :: (db `Extends` DB, Exception (ActionError db)) =>
    ThentosSessionToken -> Action db ()
endThentosSession = update'P . T.EndThentosSession


-- | Open a session for any agent.
-- NOTE: This should only be called after verifying the agent's credentials
_startThentosSessionByAgent :: (db `Extends` DB, Exception (ActionError db)) =>
    Agent -> Action db ThentosSessionToken
_startThentosSessionByAgent agent = do
    now <- getCurrentTime'P
    tok <- freshSessionToken
    update'P $ T.StartThentosSession tok agent now defaultSessionTimeout
    return tok


-- | For a thentos session, look up all service sessions and return their service names.  Requires
-- 'RoleAdmin', service, or user privs.
serviceNamesFromThentosSession :: (db `Extends` DB, Exception (ActionError db)) =>
    ThentosSessionToken -> Action db [ServiceName]
serviceNamesFromThentosSession tok = do
    now <- getCurrentTime'P

    ts :: ThentosSession
        <- snd <$> update'P (T.LookupThentosSession now tok)

    ss :: [ServiceSession]
        <- mapM (fmap snd . update'P . T.LookupServiceSession now) $
             Set.toList (ts ^. thSessServiceSessions)

    xs :: [(ServiceId, Service)]
        <- mapM (\ s -> query'P $ T.LookupService (s ^. srvSessService)) ss

    liftLIO $ guardWrite (RoleAdmin \/ ts ^. thSessAgent %% RoleAdmin /\ ts ^. thSessAgent)

    return $ (^. serviceName) . snd <$> xs


-- * service session

-- | Like 'lookupThentosSession', but for 'ServiceSession's.
lookupServiceSession :: (db `Extends` DB, Exception (ActionError db)) =>
    ServiceSessionToken -> Action db ServiceSession
lookupServiceSession tok = do
    now <- getCurrentTime'P
    session <- snd <$> update'P (T.LookupServiceSession now tok)
    let agent = ServiceA (session ^. srvSessService)
    tryTaint (RoleAdmin \/ agent %% False)
        (return session)
        (\ (_ :: AnyLabelError) -> throwError $ thentosErrorFromParent NoSuchServiceSession)

-- | Like 'existsThentosSession', but for 'ServiceSession's.
existsServiceSession :: (db `Extends` DB, Show (ActionError db)) =>
    ServiceSessionToken -> Action db Bool
existsServiceSession tok = (lookupServiceSession tok >> return True) `catchError`
    \case (thentosErrorToParent -> Just NoSuchServiceSession) -> return False
          e                                                   -> throwError e

-- | (As soon as there is a good reason, we can export this.  just need to think about the label.)
_thentosSessionAndUserIdByToken :: (db `Extends` DB, Exception (ActionError db)) =>
    ThentosSessionToken -> Action db (ThentosSession, UserId)
_thentosSessionAndUserIdByToken tok = do
    session <- lookupThentosSession tok
    case session ^. thSessAgent of
        UserA uid -> return (session, uid)
        ServiceA sid -> throwError . thentosErrorFromParent $ NeedUserA tok sid

_serviceSessionUser :: (db `Extends` DB, Exception (ActionError db)) =>
    ServiceSessionToken -> Action db UserId
_serviceSessionUser tok = do
    serviceSession <- lookupServiceSession tok
    let thentosSessionToken = serviceSession ^. srvSessThentosSession
    thentosSession <- lookupThentosSession thentosSessionToken
    case thentosSession ^. thSessAgent of
        UserA uid -> return uid
        ServiceA sid -> throwError . thentosErrorFromParent $ NeedUserA thentosSessionToken sid

-- | Register a user with a service.  Requires 'RoleAdmin' or user privs.
--
-- FIXME: We do not ask for any authorization from 'ServiceId' as of now.  It is enough to know a
-- 'ServiceId' to register with the resp. service.  This probably violates integrity of the view of
-- the service.  Fixing this may require credentials handling.  Before we do that, we should take a
-- better look at oauth.
addServiceRegistration :: (db `Extends` DB, Exception (ActionError db)) =>
    ThentosSessionToken -> ServiceId -> Action db ()
addServiceRegistration tok sid = do
    (_, uid) <- _thentosSessionAndUserIdByToken tok
    liftLIO $ guardWrite (RoleAdmin \/ UserA uid %% RoleAdmin /\  UserA uid)
    update'P $ T.UpdateUserField uid (T.UpdateUserFieldInsertService sid newServiceAccount)

-- | Undo registration of a user with a service.  Requires 'RoleAdmin' or user privs.
--
-- See FIXME in 'addServiceRegistration'.
dropServiceRegistration :: (db `Extends` DB, Exception (ActionError db)) =>
    ThentosSessionToken -> ServiceId -> Action db ()
dropServiceRegistration tok sid = do
    (_, uid) <- _thentosSessionAndUserIdByToken tok
    liftLIO $ guardWrite (RoleAdmin \/ UserA uid %% RoleAdmin /\  UserA uid)
    update'P $ T.UpdateUserField uid (T.UpdateUserFieldDropService sid)

-- | Login user running the current thentos session into service.  If user is not registered with
-- service, throw an error.
--
-- Inherits label and exception behavor from 'lookupThentosSession' and write-guards for thentos
-- session owner.
startServiceSession :: (db `Extends` DB, Exception (ActionError db)) =>
    ThentosSessionToken -> ServiceId -> Action db ServiceSessionToken
startServiceSession ttok sid = do
    now <- getCurrentTime'P
    _ <- lookupThentosSession ttok
    stok <- freshServiceSessionToken
    update'P $ T.StartServiceSession ttok stok sid now defaultSessionTimeout
    return stok

-- | Terminate service session. Throws NoSuchServiceSession if the user does not
-- own the session.
endServiceSession :: (db `Extends` DB, Exception (ActionError db)) =>
    ServiceSessionToken -> Action db ()
endServiceSession tok = do
    uid <- _serviceSessionUser tok
    tryGuardWrite (RoleAdmin \/ UserA uid %% RoleAdmin /\  UserA uid)
                  (update'P $ T.EndServiceSession tok)
                  (\ (_ :: AnyLabelError) -> throwError $ thentosErrorFromParent NoSuchServiceSession)

-- | Inherits label from 'lookupServiceSession'.
getServiceSessionMetadata :: (db `Extends` DB, Exception (ActionError db)) =>
    ServiceSessionToken -> Action db ServiceSessionMetadata
getServiceSessionMetadata tok = (^. srvSessMetadata) <$> lookupServiceSession tok


-- * agents and roles

assignRole :: (db `Extends` DB, Exception (ActionError db)) => Agent -> Role -> Action db ()
assignRole agent role = do
    liftLIO $ guardWrite (RoleAdmin %% RoleAdmin)
    update'P $ T.AssignRole agent role

unassignRole :: (db `Extends` DB, Exception (ActionError db)) => Agent -> Role -> Action db ()
unassignRole agent role = do
    liftLIO $ guardWrite (RoleAdmin %% RoleAdmin)
    update'P $ T.UnassignRole agent role

agentRoles :: (db `Extends` DB, Exception (ActionError db)) => Agent -> Action db [Role]
agentRoles agent = do
    liftLIO $ guardWrite (RoleAdmin \/ agent %% RoleAdmin /\ agent)
    Set.toList <$> query'P (T.AgentRoles agent)


-- * SSO

-- | This doesn't check any labels because it needs to be called as part of the
-- authentication process.
addNewSsoToken :: (db `Extends` DB, Exception (ActionError db)) => Action db SsoToken
addNewSsoToken = do
    tok <- freshSsoToken
    update'P $ T.AddSsoToken tok
    return tok

-- | This doesn't check any labels because it needs to be called as part of the
-- authentication process.
lookupAndRemoveSsoToken :: (db `Extends` DB, Exception (ActionError db)) =>
    SsoToken -> Action db ()
lookupAndRemoveSsoToken tok = update'P $ T.LookupAndRemoveSsoToken tok


-- * garbage collection

collectGarbage :: (db `Extends` DB, Exception (ActionError db)) => Action db ()
collectGarbage = do
    liftLIO $ guardWrite (RoleAdmin %% RoleAdmin)

    now <- getCurrentTime'P
    query'P (T.GarbageCollectThentosSessions now) >>= update'P . T.DoGarbageCollectThentosSessions
    query'P (T.GarbageCollectServiceSessions now) >>= update'P . T.DoGarbageCollectServiceSessions

    config <- getConfig'P
    let userExpiry = config >>. (Proxy :: Proxy '["user_reg_expiration"])
        passwordExpiry = config >>. (Proxy :: Proxy '["pw_reset_expiration"])
        emailExpiry = config >>. (Proxy :: Proxy '["email_change_expiration"])
    update'P $ T.DoGarbageCollectUnconfirmedUsers now userExpiry
    update'P $ T.DoGarbageCollectEmailChangeTokens now emailExpiry
    update'P $ T.DoGarbageCollectPasswordResetTokens now passwordExpiry
