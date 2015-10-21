{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeOperators        #-}

module Thentos.Action
    ( freshRandomName
    , freshConfirmationToken
    , freshPasswordResetToken
    , freshServiceId
    , freshServiceKey
    , freshSessionToken
    , freshServiceSessionToken

    , lookupConfirmedUser
    , lookupConfirmedUserByName
    , lookupConfirmedUserByEmail
    , addUser
    , deleteUser
    , addUnconfirmedUser
    , addUnconfirmedUserWithId
    , confirmNewUser
    , confirmNewUserById
    , addPasswordResetToken
    , resetPassword
    , changePassword
    , _changePasswordUnconditionally
    , requestUserEmailChange
    , confirmUserEmailChange

    , allServiceIds
    , lookupService
    , addService
    , deleteService
    , autocreateServiceIfMissing'P

    , addPersona
    , deletePersona
    , addContext
    , deleteContext
    , registerPersonaWithContext
    , unregisterPersonaFromContext
    , findPersona
    , contextsForService
    , addPersonaToGroup
    , removePersonaFromGroup
    , addGroupToGroup
    , removeGroupFromGroup
    , personaGroups

    , defaultSessionTimeout
    , lookupThentosSession
    , existsThentosSession
    , startThentosSessionByUserId
    , startThentosSessionByUserName
    , startThentosSessionByUserEmail
    , startThentosSessionByServiceId
    , endThentosSession
    , validateThentosUserSession
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

    , collectGarbage
    )
where

import Control.Conditional ((<||>))
import Control.Lens ((^.))
import Control.Monad (unless, void)
import Control.Monad.Except (throwError, catchError)
import Data.Configifier ((>>.), Tagged(Tagged))
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (ST, cs)
import Data.Typeable (Typeable)
import GHC.Exception (Exception)
import LIO.DCLabel ((%%), (\/), (/\))
import LIO.Error (AnyLabelError)
import System.Log.Logger (Priority(DEBUG))

import qualified Codec.Binary.Base64 as Base64
import qualified Data.Text as ST

import LIO.Missing
import Thentos.Action.Core
import Thentos.Action.SimpleAuth
import Thentos.Types
import Thentos.Util

import qualified Thentos.Transaction as T
import Thentos.Transaction.Core (ThentosQuery)


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
freshRandomName :: Action e ST
freshRandomName = ST.replace "/" "_" . cs . Base64.encode <$> genRandomBytes'P 18

freshConfirmationToken :: Action e ConfirmationToken
freshConfirmationToken = ConfirmationToken <$> freshRandomName

freshPasswordResetToken :: Action e PasswordResetToken
freshPasswordResetToken = PasswordResetToken <$> freshRandomName

freshServiceId :: Action e ServiceId
freshServiceId = ServiceId <$> freshRandomName

freshServiceKey :: Action e ServiceKey
freshServiceKey = ServiceKey <$> freshRandomName

freshSessionToken :: Action e ThentosSessionToken
freshSessionToken = ThentosSessionToken <$> freshRandomName

freshServiceSessionToken :: Action e ServiceSessionToken
freshServiceSessionToken = ServiceSessionToken <$> freshRandomName


-- * user

-- | Return a user with its id.  Requires or privileges of admin or the user that is looked up.  If
-- no user is found or access is not granted, throw 'NoSuchUser'.  See '_lookupUserCheckPassword' for
-- user lookup prior to authentication.
lookupConfirmedUser :: UserId -> Action e (UserId, User)
lookupConfirmedUser uid = _lookupUser $ T.lookupConfirmedUser uid

_lookupUser :: ThentosQuery e (UserId, User) -> Action e (UserId, User)
_lookupUser transaction = do
    val@(uid, _) <- query'P transaction
    tryTaint (RoleAdmin \/ UserA uid %% False)
        (return val)
        (\ (_ :: AnyLabelError) -> throwError NoSuchUser)

-- | Like 'lookupConfirmedUser', but based on 'UserName'.
lookupConfirmedUserByName :: UserName -> Action e (UserId, User)
lookupConfirmedUserByName name = _lookupUser $ T.lookupConfirmedUserByName name

-- | Like 'lookupConfirmedUser', but based on 'UserEmail'.
lookupConfirmedUserByEmail :: UserEmail -> Action e (UserId, User)
lookupConfirmedUserByEmail email = _lookupUser $ T.lookupConfirmedUserByEmail email

-- | Add a user based on its form data.  Requires 'RoleAdmin'.  For creating users with e-mail
-- verification, see 'addUnconfirmedUser', 'confirmNewUser'.
addUser :: (Show e, Typeable e) => UserFormData -> Action e UserId
addUser userData = do
    guardWriteMsg "addUser" (RoleAdmin %% RoleAdmin)
    makeUserFromFormData'P userData >>= query'P . T.addUser

-- | Delete user.  Requires or privileges of admin or the user that is looked up.  If no user is
-- found or access is not granted, throw 'NoSuchUser'.
deleteUser :: UserId -> Action e ()
deleteUser uid = do
    guardWriteMsg "deleteUser" (RoleAdmin \/ UserA uid %% RoleAdmin /\ UserA uid)
    query'P $ T.deleteUser uid


-- ** email confirmation

-- | Initiate email-verified user creation.  Does not require any privileges.  See also:
-- 'confirmNewUser'.
addUnconfirmedUser :: (Show e, Typeable e) => UserFormData -> Action e (UserId, ConfirmationToken)
addUnconfirmedUser userData = do
    tok  <- freshConfirmationToken
    user <- makeUserFromFormData'P userData
    uid  <- query'P $ T.addUnconfirmedUser tok user
    return (uid, tok)

-- | Initiate email-verified user creation, assigning a specific ID to the new user.
-- If the ID is already in use, an error is thrown. Does not require any privileges.
addUnconfirmedUserWithId :: UserFormData -> UserId -> Action e ConfirmationToken
addUnconfirmedUserWithId userData userId = do
    tok  <- freshConfirmationToken
    user <- makeUserFromFormData'P userData
    query'P $ T.addUnconfirmedUserWithId tok user userId
    return tok

-- | Finish email-verified user creation.
--
-- SECURITY: As a caller, you have to make sure the token has been produced by the legitimate
-- recipient of a confirmation email.  Authentication can only be provided by this api **after** the
-- user has been created by calling this function.
--
-- See also: 'addUnconfirmedUser'.
confirmNewUser :: ConfirmationToken -> Action e (UserId, ThentosSessionToken)
confirmNewUser token = do
    expiryPeriod <- (>>. (Proxy :: Proxy '["user_reg_expiration"])) <$> getConfig'P
    uid <- query'P $ T.finishUserRegistration expiryPeriod token
    sessionToken <- _startThentosSessionByAgent (UserA uid)
    return (uid, sessionToken)


-- | Finish email-verified user creation, identifying the user by their 'UserId' and ignoring the
-- 'ConfirmationToken'.
--
-- SECURITY: As a caller, you have to make sure that the user credentials have indeed been properly
-- verified before calling this function. This function accepts that as a fact, but cannot in any
-- way check it.
--
-- See also: 'addUnconfirmedUser'.
confirmNewUserById :: UserId -> Action e ThentosSessionToken
confirmNewUserById uid = do
    query'P $ T.finishUserRegistrationById uid
    _startThentosSessionByAgent (UserA uid)


-- ** password reset

-- | Initiate password reset with email confirmation.  No authentication required, obviously.
addPasswordResetToken :: UserEmail -> Action e (User, PasswordResetToken)
addPasswordResetToken email = do
    tok <- freshPasswordResetToken
    user <- query'P $ T.addPasswordResetToken email tok
    return (user, tok)

-- | Finish password reset with email confirmation.
--
-- SECURITY: See 'confirmNewUser'.
resetPassword :: PasswordResetToken -> UserPass -> Action e ()
resetPassword token password = do
    expiryPeriod <- (>>. (Proxy :: Proxy '["pw_reset_expiration"])) <$> getConfig'P
    hashedPassword <- hashUserPass'P password
    query'P $ T.resetPassword expiryPeriod token hashedPassword


-- ** login

-- | Find user running the action, confirm the password, and return the user or crash.
-- 'NoSuchUser' is translated into 'BadCredentials'.
-- NOTE: This should not be exported from this module, as it allows access to
-- the user map without any clearance.
_lookupUserCheckPassword ::
    ThentosQuery e (UserId, User) -> UserPass -> Action e (UserId, User)
_lookupUserCheckPassword transaction password = a `catchError` h
  where
    a = do
        (uid, user) <- query'P transaction
        if verifyPass password user
            then return (uid, user)
            else throwError BadCredentials

    h NoSuchUser = throwError BadCredentials
    h e          = throwError e


-- ** change user data

-- | Authenticate user against old password, and then change password to new password.
--
-- LIO policy: In addition to the old password as proof of authority, this function requires the
-- user to change the password to be logged in (or admin privs).
changePassword :: UserId -> UserPass -> UserPass -> Action e ()
changePassword uid old new = do
    _ <- _lookupUserCheckPassword (T.lookupAnyUser uid) old
    hashedPw <- hashUserPass'P new
    guardWriteMsg "changePassword" (RoleAdmin \/ UserA uid %% RoleAdmin /\ UserA uid)
    query'P $ T.changePassword uid hashedPw

-- FIXME: As the '_' says, this function shouldn't be exported, but wrapped in a public action that
-- establishes that the password change is legitimate.  (Currently, this function is only called in
-- "Thentos.Adhocracy3.Backend.Api.Simple", and that will change heavily during implementation of
-- #321.  If we would keep the current setup, we would pull the code calling the service into the
-- wrapping action, and taint that with the obtained user id.  Once #321 has been implemented, we
-- should have something analogous happening here in this module.)
_changePasswordUnconditionally :: UserId -> UserPass -> Action e ()
_changePasswordUnconditionally uid newPw = do
    hashedPw <- hashUserPass'P newPw
    query'P $ T.changePassword uid hashedPw

-- | Initiate email change by creating and storing a token and sending it out by email to the old
-- address of the user.  This requires 'RoleAdmin' or privs of email address owner, but the address
-- is only changed after a call to 'confirmUserEmailChange' with the correct token.
requestUserEmailChange ::
    UserId -> UserEmail -> (ConfirmationToken -> ST) -> Action e ()
requestUserEmailChange uid newEmail callbackUrlBuilder = do
    guardWriteMsg "requestUserEmailChange" (RoleAdmin \/ UserA uid %% RoleAdmin /\ UserA uid)

    tok <- freshConfirmationToken
    smtpConfig <- (Tagged . (>>. (Proxy :: Proxy '["smtp"]))) <$> getConfig'P

    query'P $ T.addUserEmailChangeRequest uid newEmail tok

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
confirmUserEmailChange :: ConfirmationToken -> Action e ()
confirmUserEmailChange token = do
    expiryPeriod <- (>>. (Proxy :: Proxy '["email_change_expiration"])) <$> getConfig'P
    void . query'P $ T.confirmUserEmailChange expiryPeriod token


-- * service

allServiceIds :: Action e [ServiceId]
allServiceIds = do
    taintMsg "allServiceIds" (RoleAdmin %% False)
    query'P T.allServiceIds

lookupService :: ServiceId -> Action e (ServiceId, Service)
lookupService sid = query'P $ T.lookupService sid

addService ::
    UserId -> ServiceName -> ServiceDescription -> Action e (ServiceId, ServiceKey)
addService owner name desc = do
    sid <- freshServiceId
    addServicePrim owner sid name desc

addServicePrim ::
    UserId -> ServiceId -> ServiceName -> ServiceDescription -> Action e (ServiceId, ServiceKey)
addServicePrim owner sid name desc = do
    assertAuth (hasUserId owner <||> hasRole RoleAdmin)
    key <- freshServiceKey
    hashedKey <- hashServiceKey'P key
    query'P $ T.addService owner sid hashedKey name desc
    return (sid, key)

deleteService :: ServiceId -> Action e ()
deleteService sid = do
    owner <- (^. serviceOwner) . snd <$> lookupService sid
    assertAuth (hasUserId owner <||> hasRole RoleAdmin)
    query'P $ T.deleteService sid

-- | Autocreate a service with a specific ID if it doesn't exist yet. This allows adding services
-- to the config which will automatically spring into life if the config is read.
autocreateServiceIfMissing'P :: UserId -> ServiceId -> Action e ()
autocreateServiceIfMissing'P owner sid = void (lookupService sid) `catchError`
    \case NoSuchService -> do
            logger'P DEBUG $ "autocreating service with ID " ++ show sid
            void $ addServicePrim owner sid (ServiceName "autocreated")
                                  (ServiceDescription "autocreated")
          e -> throwError e


-- * thentos session

-- | Thentos and service sessions have a fixed duration of 2 weeks.
--
-- FIXME: make configurable, and distinguish between thentos sessions and service sessions.
-- (eventually, this will need to be run-time configurable.  but there will probably still be global
-- defaults handled by configifier.)
defaultSessionTimeout :: Timeout
defaultSessionTimeout = fromDays 14

-- | Find 'ThentosSession' from token.  If 'ThentosSessionToken' does not exist or clearance does
-- not allow access, throw 'NoSuchThentosSession'.
lookupThentosSession :: ThentosSessionToken -> Action e ThentosSession
lookupThentosSession tok = do
    session <- _lookupThentosSession tok
    tryTaint (RoleAdmin \/ session ^. thSessAgent %% False)
        (return session)
        (\ (_ :: AnyLabelError) -> throwError NoSuchThentosSession)

-- | Find 'ThentosSession' from token, without clearance check.
_lookupThentosSession :: ThentosSessionToken -> Action e ThentosSession
_lookupThentosSession tok = snd <$> query'P (T.lookupThentosSession tok)

-- | Like 'lookupThentosSession', but does not throw an exception if thentos session does not exist
-- or is inaccessible, but returns 'False' instead.
existsThentosSession :: ThentosSessionToken -> Action e Bool
existsThentosSession tok = (lookupThentosSession tok >> return True) `catchError`
    \case NoSuchThentosSession -> return False
          e                    -> throwError e

-- | Check user credentials and create a session for user.  Requires 'lookupConfirmedUser' and
-- '_startThentosSessionByAgent'.
startThentosSessionByUserId :: UserId -> UserPass -> Action e ThentosSessionToken
startThentosSessionByUserId uid pass = do
    _ <- _lookupUserCheckPassword (T.lookupConfirmedUser uid) pass
    _startThentosSessionByAgent (UserA uid)

-- | Like 'startThentosSessionByUserId', but based on 'UserName' as key.
startThentosSessionByUserName ::
    UserName -> UserPass -> Action e (UserId, ThentosSessionToken)
startThentosSessionByUserName name pass = do
    (uid, _) <- _lookupUserCheckPassword (T.lookupConfirmedUserByName name) pass
    (uid,) <$> _startThentosSessionByAgent (UserA uid)

startThentosSessionByUserEmail ::
    UserEmail -> UserPass -> Action e (UserId, ThentosSessionToken)
startThentosSessionByUserEmail email pass = do
    (uid, _) <- _lookupUserCheckPassword (T.lookupConfirmedUserByEmail email) pass
    (uid,) <$> _startThentosSessionByAgent (UserA uid)

-- | Check service credentials and create a session for service.
startThentosSessionByServiceId ::
    ServiceId -> ServiceKey -> Action e ThentosSessionToken
startThentosSessionByServiceId sid key = a `catchError` h
  where
    a = do
        (_, service) <- query'P (T.lookupService sid)
        unless (verifyKey key service) $ throwError BadCredentials
        _startThentosSessionByAgent (ServiceA sid)

    h NoSuchService =
        throwError BadCredentials
    h e = throwError e

-- | Terminate 'ThentosSession'.  Does not require any label; being in possession of the session
-- token is enough authentication to terminate it.
endThentosSession :: ThentosSessionToken -> Action e ()
endThentosSession = query'P . T.endThentosSession

-- | Check that a Thentos session exists, is not expired, and belongs to a user (rather than a
-- service). Returns information on the user if that's the case. Throws 'NoSuchThentosSession'
-- otherwise.
--
-- We assume that the ThentosSessionToken is a secret that nobody except the session owner can
-- know, therefore no special clearance is required.
validateThentosUserSession :: ThentosSessionToken -> Action e (UserId, User)
validateThentosUserSession tok = do
    session <- _lookupThentosSession tok
    case session ^. thSessAgent of
        UserA uid  -> query'P $ T.lookupConfirmedUser uid
        ServiceA _ -> throwError NoSuchThentosSession

-- | Open a session for any agent.
-- NOTE: This should only be called after verifying the agent's credentials
_startThentosSessionByAgent :: Agent -> Action e ThentosSessionToken
_startThentosSessionByAgent agent = do
    tok <- freshSessionToken
    query'P $ T.startThentosSession tok agent defaultSessionTimeout
    return tok

-- | For a thentos session, look up all service sessions and return their service names.  Requires
-- 'RoleAdmin', service, or user privs.
serviceNamesFromThentosSession :: ThentosSessionToken -> Action e [ServiceName]
serviceNamesFromThentosSession tok = do
    lookupThentosSession tok >>= \sess -> assertAuth $ hasAgent (sess ^. thSessAgent)
    query'P $ T.serviceNamesFromThentosSession tok


-- * service session

-- | Like 'lookupThentosSession', but for 'ServiceSession's.
lookupServiceSession :: ServiceSessionToken -> Action e ServiceSession
lookupServiceSession tok = snd <$> query'P (T.lookupServiceSession tok)

-- | Like 'existsThentosSession', but for 'ServiceSession's.
existsServiceSession :: ServiceSessionToken -> Action e Bool
existsServiceSession tok = (lookupServiceSession tok >> return True) `catchError`
    \case NoSuchServiceSession -> return False
          e                    -> throwError e

-- | (As soon as there is a good reason, we can export this.  just need to think about the label.)
_thentosSessionAndUserIdByToken ::
    ThentosSessionToken -> Action e (ThentosSession, UserId)
_thentosSessionAndUserIdByToken tok = do
    session <- lookupThentosSession tok
    case session ^. thSessAgent of
        UserA uid -> return (session, uid)
        ServiceA sid -> throwError $ NeedUserA tok sid

_serviceSessionUser :: ServiceSessionToken -> Action e UserId
_serviceSessionUser tok = do
    serviceSession <- lookupServiceSession tok
    let thentosSessionToken = serviceSession ^. srvSessThentosSession
    thentosSession <- _lookupThentosSession thentosSessionToken
    case thentosSession ^. thSessAgent of
        UserA uid -> return uid
        ServiceA sid -> throwError $ NeedUserA thentosSessionToken sid

-- | Register a user with a service.  Requires 'RoleAdmin' or user privs.
--
-- FIXME: We do not ask for any authorization from 'ServiceId' as of now.  It is enough to know a
-- 'ServiceId' to register with the resp. service.  This probably violates integrity of the view of
-- the service.  Fixing this may require credentials handling.  Before we do that, we should take a
-- better look at oauth.
addServiceRegistration :: ThentosSessionToken -> ServiceId -> Action e ()
addServiceRegistration tok sid = do
    (_, uid) <- _thentosSessionAndUserIdByToken tok
    query'P $ T.registerUserWithService uid sid newServiceAccount

-- | Undo registration of a user with a service.  Requires 'RoleAdmin' or user privs.
--
-- See FIXME in 'addServiceRegistration'.
dropServiceRegistration :: ThentosSessionToken -> ServiceId -> Action e ()
dropServiceRegistration tok sid = do
    (_, uid) <- _thentosSessionAndUserIdByToken tok
    guardWriteMsg "dropServiceRegistration"
        (RoleAdmin \/ UserA uid %% RoleAdmin /\ UserA uid)
    query'P $ T.unregisterUserFromService uid sid

-- | Login user running the current thentos session into service.  If user is not registered with
-- service, throw an error.
--
-- Inherits label and exception behavor from 'lookupThentosSession' and write-guards for thentos
-- session owner.
startServiceSession ::
    ThentosSessionToken -> ServiceId -> Action e ServiceSessionToken
startServiceSession ttok sid = do
    _ <- lookupThentosSession ttok
    stok <- freshServiceSessionToken
    query'P $ T.startServiceSession ttok stok sid defaultSessionTimeout
    return stok

-- | Terminate service session. Throws NoSuchServiceSession if the user does not
-- own the session.
endServiceSession :: ServiceSessionToken -> Action e ()
endServiceSession tok = query'P $ T.endServiceSession tok

-- | Inherits label from 'lookupServiceSession'.
getServiceSessionMetadata :: ServiceSessionToken -> Action e ServiceSessionMetadata
getServiceSessionMetadata tok = (^. srvSessMetadata) <$> lookupServiceSession tok


-- * personas, contexts, and groups

-- | Add a new persona to the DB. A persona has a unique name and a user to which it belongs.
-- The 'PersonaId' is assigned by the DB. May throw 'NoSuchUser' or 'PersonaNameAlreadyExists'.
-- Only the user owning the persona or an admin may do this.
addPersona :: PersonaName -> UserId -> Action e Persona
addPersona name uid = do
    assertAuth (hasUserId uid <||> hasRole RoleAdmin)
    query'P $ T.addPersona name uid

-- | Delete a persona. Throw 'NoSuchPersona' if the persona does not exist in the DB.
-- Only the user owning the persona or an admin may do this.
deletePersona :: Persona -> Action e ()
deletePersona persona = do
    assertAuth (hasUserId (persona ^. personaUid) <||> hasRole RoleAdmin)
    query'P . T.deletePersona $ persona ^. personaId

-- | Add a new context. The first argument identifies the service to which the context belongs.
-- May throw 'NoSuchService' or 'ContextNameAlreadyExists'.
-- Only the service or an admin may do this.
addContext :: ServiceId -> ContextName -> ContextDescription -> ProxyUri -> Action e Context
addContext sid name desc url = do
    assertAuth (hasServiceId sid <||> hasRole RoleAdmin)
    query'P $ T.addContext sid name desc url

-- | Delete a context. Throw an error if the context does not exist in the DB.
-- Only the service owning the context or an admin may do this.
deleteContext :: Context -> Action e ()
deleteContext context = do
    assertAuth (hasServiceId (context ^. contextService) <||> hasRole RoleAdmin)
    query'P . T.deleteContext $ context ^. contextId

-- | Connect a persona with a context. Throws an error if the persona is already registered for the
-- context or if the user has any *other* persona registered for the context
-- ('MultiplePersonasPerContext'). (As we currently allow only one persona per user and context.)
-- Throws 'NoSuchPersona' or 'NoSuchContext' if one of the arguments doesn't exist.
-- Only the user owning the persona or an admin may do this.
registerPersonaWithContext :: Persona -> ContextId -> Action e ()
registerPersonaWithContext persona cxtId = do
    assertAuth (hasUserId (persona ^. personaUid) <||> hasRole RoleAdmin)
    query'P $ T.registerPersonaWithContext persona cxtId

-- | Unregister a persona from accessing a context. No-op if the persona was not registered for the
-- context. Only the user owning the persona or an admin may do this.
unregisterPersonaFromContext :: Persona -> ContextId -> Action e ()
unregisterPersonaFromContext persona cxtId = do
    assertAuth (hasUserId (persona ^. personaUid) <||> hasRole RoleAdmin)
    query'P $ T.unregisterPersonaFromContext (persona ^. personaId) cxtId

-- | Find the persona that a user wants to use for a context (if any).
-- Only the user owning the persona or an admin may do this.
findPersona :: UserId -> ContextId -> Action e (Maybe Persona)
findPersona uid cxtId = do
    assertAuth (hasUserId uid <||> hasRole RoleAdmin)
    query'P $ T.findPersona uid cxtId

-- | List all contexts owned by a service. Anybody may do this.
contextsForService :: ServiceId -> Action e [Context]
contextsForService sid = query'P $ T.contextsForService sid

-- | Add a persona to a group. If the persona is already a member of the group, do nothing.
-- Only a GroupAdmin may do this.
addPersonaToGroup :: PersonaId -> Group -> Action e ()
addPersonaToGroup pid group = do
    assertAuth $ hasRole RoleGroupAdmin
    query'P $ T.addPersonaToGroup pid group

-- | Remove a persona from a group. If the persona is not a member of the group, do nothing.
-- Only a GroupAdmin may do this.
removePersonaFromGroup :: PersonaId -> Group -> Action e ()
removePersonaFromGroup pid group = do
    assertAuth $ hasRole RoleGroupAdmin
    query'P $ T.removePersonaFromGroup pid group

-- | Add a group (subgroup) to another group (supergroup) so that all members of subgroup will also
-- be considered members of supergroup. If subgroup is already a direct member of supergroup, do
-- nothing. Throws 'GroupMembershipLoop' if adding the relation would cause a loop.
-- Only a GroupAdmin may do this.
addGroupToGroup :: Group -> Group -> Action e ()
addGroupToGroup subgroup supergroup = do
    assertAuth $ hasRole RoleGroupAdmin
    query'P $ T.addGroupToGroup subgroup supergroup

-- | Remove a group (subgroup) from another group (supergroup). If subgroup is not a direct
-- member of supergroup, do nothing. Only a GroupAdmin may do this.
removeGroupFromGroup :: Group -> Group -> Action e ()
removeGroupFromGroup subgroup supergroup = do
    assertAuth $ hasRole RoleGroupAdmin
    query'P $ T.removeGroupFromGroup subgroup supergroup

-- | List all groups a persona belongs to, directly or indirectly. If p is a member of g1,
-- g1 is a member of g2, and g2 is a member of g3, [g1, g2, g3] will be returned.
-- Only the user owning the persona or a GroupAdmin may do this.
personaGroups :: Persona -> Action e [Group]
personaGroups persona = do
    assertAuth $ hasUserId (persona ^. personaUid) <||> hasRole RoleGroupAdmin
    query'P $ T.personaGroups (persona ^. personaId)


-- * agents and roles

assignRole :: Agent -> Role -> Action e ()
assignRole agent role = do
    guardWriteMsg "assignRole" (RoleAdmin %% RoleAdmin)
    query'P $ T.assignRole agent role

unassignRole :: Agent -> Role -> Action e ()
unassignRole agent role = do
    guardWriteMsg "unassignRole" (RoleAdmin %% RoleAdmin)
    query'P $ T.unassignRole agent role

agentRoles :: Agent -> Action e [Role]
agentRoles agent = do
    taintMsg "agentRoles" (RoleAdmin \/ agent %% RoleAdmin /\ agent)
    query'P (T.agentRoles agent)


-- * garbage collection

collectGarbage :: Exception (ActionError e) => Action e ()
collectGarbage = do
    logger'P DEBUG "starting garbage collection."
    guardWriteMsg "collectGarbage" (RoleAdmin %% RoleAdmin)

    query'P T.garbageCollectThentosSessions
    query'P T.garbageCollectServiceSessions

    config <- getConfig'P
    let userExpiry = config >>. (Proxy :: Proxy '["user_reg_expiration"])
        passwordExpiry = config >>. (Proxy :: Proxy '["pw_reset_expiration"])
        emailExpiry = config >>. (Proxy :: Proxy '["email_change_expiration"])
    query'P $ T.garbageCollectUnconfirmedUsers userExpiry
    query'P $ T.garbageCollectEmailChangeTokens emailExpiry
    query'P $ T.garbageCollectPasswordResetTokens passwordExpiry

    logger'P DEBUG "garbage collection complete!"
