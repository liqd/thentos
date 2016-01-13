{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}

module Thentos.Action
    ( freshRandomName
    , freshRandom20
    , freshConfirmationToken
    , freshPasswordResetToken
    , freshServiceId
    , freshServiceKey
    , freshSessionToken
    , freshServiceSessionToken
    , freshCaptchaId

    , lookupConfirmedUser
    , lookupConfirmedUserByName
    , lookupConfirmedUserByEmail
    , addUser
    , deleteUser
    , addUnconfirmedUser
    , addUnconfirmedUserWithCaptcha
    , confirmNewUser
    , addPasswordResetToken
    , resetPassword
    , changePassword
    , changePasswordUnconditionally_
    , requestUserEmailChange
    , confirmUserEmailChange

    , allServiceIds
    , lookupService
    , addService
    , deleteService
    , autocreateServiceIfMissing

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

    , makeCaptcha
    , makeAudioCaptcha
    , solveCaptcha
    , deleteCaptcha

    , collectGarbage
    )
where

import Control.Conditional ((<||>), unlessM)
import Control.Lens ((^.))
import Control.Monad (unless, void, when)
import Control.Monad.Except (throwError, catchError)
import Data.Configifier ((>>.), Tagged(Tagged))
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (ST, SBS, cs)
import Data.Typeable (Typeable)
import GHC.Exception (Exception)
import LIO.DCLabel ((%%), (\/), (/\))
import LIO.Error (AnyLabelError(AnyLabelError))
import System.Log.Logger (Priority(DEBUG))
import Text.Hastache.Context (mkStrContext)
import Text.Hastache (MuType(MuVariable))

import qualified Codec.Binary.Base64 as Base64
import qualified Data.Text as ST

import LIO.Missing
import Thentos.Action.Core
import Thentos.Action.Types
import Thentos.Action.SimpleAuth
import Thentos.Config
import Thentos.Transaction.Core (ThentosQuery)
import Thentos.Types
import Thentos.Util

import qualified Thentos.Action.Unsafe as U
import qualified Thentos.Sybil as Sybil
import qualified Thentos.Transaction as T


queryA :: ThentosQuery e a -> Action e s a
queryA = U.unsafeAction . U.query

loggerA :: Priority -> String -> Action e s ()
loggerA prio = U.unsafeAction . U.logger prio


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
freshRandomName :: Action e s ST
freshRandomName = ST.replace "/" "_" . cs . Base64.encode <$> U.unsafeAction (U.genRandomBytes 18)

freshConfirmationToken :: Action e s ConfirmationToken
freshConfirmationToken = ConfirmationToken <$> freshRandomName

-- | Generate 20 bytes of random data.
-- For comparison: an UUID has 16 bytes, so that should be enough for all practical purposes.
freshRandom20 :: Action e s Random20
freshRandom20 = do
    bytes <- U.unsafeAction $ U.genRandomBytes 20
    maybe (error "freshRandom20: internal error") pure $ mkRandom20 bytes

freshPasswordResetToken :: Action e s PasswordResetToken
freshPasswordResetToken = PasswordResetToken <$> freshRandomName

freshServiceId :: Action e s ServiceId
freshServiceId = ServiceId <$> freshRandomName

freshServiceKey :: Action e s ServiceKey
freshServiceKey = ServiceKey <$> freshRandomName

freshSessionToken :: Action e s ThentosSessionToken
freshSessionToken = ThentosSessionToken <$> freshRandomName

freshServiceSessionToken :: Action e s ServiceSessionToken
freshServiceSessionToken = ServiceSessionToken <$> freshRandomName

freshCaptchaId :: Action e s CaptchaId
freshCaptchaId = CaptchaId <$> freshRandomName


-- * user

-- | Return a user with its id.  Requires or privileges of admin or the user that is looked up.  If
-- no user is found or access is not granted, throw 'NoSuchUser'.  See 'lookupUserCheckPassword_' for
-- user lookup prior to authentication.
lookupConfirmedUser :: UserId -> Action e s (UserId, User)
lookupConfirmedUser uid = lookupUser_ $ T.lookupConfirmedUser uid

lookupUser_ :: ThentosQuery e (UserId, User) -> Action e s (UserId, User)
lookupUser_ transaction = do
    val@(uid, _) <- queryA transaction
    tryTaint (RoleAdmin \/ UserA uid %% False)
        (return val)
        (\ (AnyLabelError _) -> throwError NoSuchUser)

-- | Like 'lookupConfirmedUser', but based on 'UserName'.
lookupConfirmedUserByName :: UserName -> Action e s (UserId, User)
lookupConfirmedUserByName name = lookupUser_ $ T.lookupConfirmedUserByName name

-- | Like 'lookupConfirmedUser', but based on 'UserEmail'.
lookupConfirmedUserByEmail :: UserEmail -> Action e s (UserId, User)
lookupConfirmedUserByEmail email = lookupUser_ $ T.lookupConfirmedUserByEmail email

-- | Add a user based on its form data.  Requires 'RoleAdmin'.  For creating users with e-mail
-- verification, see 'addUnconfirmedUser', 'confirmNewUser'.
addUser :: (Show e, Typeable e) => UserFormData -> Action e s UserId
addUser userData = do
    guardWriteMsg "addUser" (RoleAdmin %% RoleAdmin)
    U.unsafeAction $ U.makeUserFromFormData userData >>= U.query . T.addUser

-- | Delete user.  Requires or privileges of admin or the user that is looked up.  If no user is
-- found or access is not granted, throw 'NoSuchUser'.
deleteUser :: UserId -> Action e s ()
deleteUser uid = do
    guardWriteMsg "deleteUser" (RoleAdmin \/ UserA uid %% RoleAdmin /\ UserA uid)
    queryA $ T.deleteUser uid


-- ** email confirmation

-- | Initiate email-verified user creation.  Does not require any privileges.
-- This also sends an email containing an activation link on which the user must click.
-- See also: 'confirmNewUser'.
addUnconfirmedUser :: (Show e, Typeable e) => UserFormData -> Action e s ()
addUnconfirmedUser userData = do
    tok  <- freshConfirmationToken
    user <- U.unsafeAction $ U.makeUserFromFormData userData
    let happy = do
            void . queryA $ T.addUnconfirmedUser tok user
            sendUserConfirmationMail userData tok
    happy
        `catchError`
            \case UserEmailAlreadyExists -> sendUserExistsMail (udEmail userData)
                  e                      -> throwError e

-- | Helper action: Send a confirmation mail to a newly registered user.
sendUserConfirmationMail :: UserFormData -> ConfirmationToken -> Action e s ()
sendUserConfirmationMail user (ConfirmationToken confToken) = do
    cfg <- U.unsafeAction U.getConfig
    let subject = cfg >>. (Proxy :: Proxy '["email_templates", "account_verification", "subject"])
        bodyTemplate = cfg >>. (Proxy :: Proxy '["email_templates", "account_verification", "body"])
        feHttp = case cfg >>. (Proxy :: Proxy '["frontend"]) of
            Nothing -> error "sendUserConfirmationMail: frontend not configured!"
            Just v -> Tagged v
        context "user_name"      = MuVariable . fromUserName $ udName user
        context "activation_url" = MuVariable $ exposeUrl feHttp <//> "/activate/" <//> confToken
        context _                = error "sendUserConfirmationMail: no such context"
    body <- U.unsafeAction $ U.renderTextTemplate bodyTemplate (mkStrContext context)
    U.unsafeAction $ U.sendMail (Just $ udName user) (udEmail user) subject (cs body)

-- | Helper action: Send a confirmation mail to a newly registered user.
sendUserExistsMail :: UserEmail -> Action e s ()
sendUserExistsMail email = do
    cfg <- U.unsafeAction U.getConfig
    let subject = cfg >>. (Proxy :: Proxy '["email_templates", "user_exists", "subject"])
        body    = cfg >>. (Proxy :: Proxy '["email_templates", "user_exists", "body"])
    U.unsafeAction $ U.sendMail Nothing email subject body

-- | Initiate email-verified user creation.  Does not require any privileges, but the user must
-- have correctly solved a captcha to prove that they are human.  After the new user has been
-- created, the captcha is deleted to prevent an attacker from creating multiple users with
-- the same captcha solution.  If user creation fails (e.g. because of a duplicate user name), the
-- captcha remains in the DB to allow another attempt.  See also: 'makeCaptcha', 'confirmNewUser'.
addUnconfirmedUserWithCaptcha :: (Show e, Typeable e) => UserCreationRequest -> Action e s ()
addUnconfirmedUserWithCaptcha ucr = do
    unlessM (solveCaptcha (csId $ ucCaptcha ucr) (csSolution $ ucCaptcha ucr)) $
        throwError InvalidCaptchaSolution
    addUnconfirmedUser (ucUser ucr)
    deleteCaptcha . csId $ ucCaptcha ucr

-- | Finish email-verified user creation. Throws 'NoSuchPendingUserConfirmation' if the
-- token doesn't exist or is expired.
--
-- SECURITY: As a caller, you have to make sure the token has been produced by the legitimate
-- recipient of a confirmation email.  Authentication can only be provided by this api **after** the
-- user has been created by calling this function.
--
-- See also: 'addUnconfirmedUser'.
confirmNewUser :: ConfirmationToken -> Action e s (UserId, ThentosSessionToken)
confirmNewUser token = do
    expiryPeriod <- (>>. (Proxy :: Proxy '["user_reg_expiration"])) <$> U.unsafeAction U.getConfig
    uid <- queryA $ T.finishUserRegistration expiryPeriod token
    sessionToken <- startThentosSessionByAgent_ (UserA uid)
    return (uid, sessionToken)


-- ** password reset

-- | Initiate password reset with email confirmation.  No authentication required, obviously.
addPasswordResetToken :: UserEmail -> Action e s (User, PasswordResetToken)
addPasswordResetToken email = do
    tok <- freshPasswordResetToken
    user <- queryA $ T.addPasswordResetToken email tok
    return (user, tok)

-- | Finish password reset with email confirmation.
--
-- SECURITY: See 'confirmNewUser'.
resetPassword :: PasswordResetToken -> UserPass -> Action e s ()
resetPassword token password = do
    expiryPeriod <- (>>. (Proxy :: Proxy '["pw_reset_expiration"])) <$> U.unsafeAction U.getConfig
    hashedPassword <- U.unsafeAction $ U.hashUserPass password
    queryA $ T.resetPassword expiryPeriod token hashedPassword


-- ** login

-- | Find user running the action, confirm the password, and return the user or crash.
-- 'NoSuchUser' is translated into 'BadCredentials'.
-- NOTE: This should not be exported from this module, as it allows access to
-- the user map without any clearance.
lookupUserCheckPassword_ ::
    ThentosQuery e (UserId, User) -> UserPass -> Action e s (UserId, User)
lookupUserCheckPassword_ transaction password = a `catchError` h
  where
    a = do
        (uid, user) <- queryA transaction
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
changePassword :: UserId -> UserPass -> UserPass -> Action e s ()
changePassword uid old new = do
    _ <- lookupUserCheckPassword_ (T.lookupAnyUser uid) old
    hashedPw <- U.unsafeAction $ U.hashUserPass new
    guardWriteMsg "changePassword" (RoleAdmin \/ UserA uid %% RoleAdmin /\ UserA uid)
    queryA $ T.changePassword uid hashedPw

-- BUG #407: As the '_' says, this function shouldn't be exported, but wrapped in a public action
-- that establishes that the password change is legitimate.  (Currently, this function is only
-- called in "Thentos.Adhocracy3.Backend.Api.Simple", and that will change heavily during
-- implementation of #321.  If we would keep the current setup, we would pull the code calling the
-- service into the wrapping action, and taint that with the obtained user id.  Once #321 has been
-- implemented, we should have something analogous happening here in this module.)
changePasswordUnconditionally_ :: UserId -> UserPass -> Action e s ()
changePasswordUnconditionally_ uid newPw = do
    hashedPw <- U.unsafeAction $ U.hashUserPass newPw
    queryA $ T.changePassword uid hashedPw

-- | Initiate email change by creating and storing a token and sending it out by email to the old
-- address of the user.  This requires 'RoleAdmin' or privs of email address owner, but the address
-- is only changed after a call to 'confirmUserEmailChange' with the correct token.
requestUserEmailChange ::
    UserId -> UserEmail -> (ConfirmationToken -> ST) -> Action e s ()
requestUserEmailChange uid newEmail callbackUrlBuilder = do
    guardWriteMsg "requestUserEmailChange" (RoleAdmin \/ UserA uid %% RoleAdmin /\ UserA uid)

    tok <- freshConfirmationToken
    queryA $ T.addUserEmailChangeRequest uid newEmail tok

    let message = "Please go to " <> callbackUrlBuilder tok <> " to confirm your change of email address."
        subject = "Thentos email address change"
    U.unsafeAction $ U.sendMail Nothing newEmail subject message

-- | Look up the given confirmation token and updates the user's email address iff 1) the token
-- exists, 2) the token belongs to the user currently logged in, and 3) the token has not
-- expired. If any of these conditions don't apply, throw 'NoSuchToken' to avoid leaking
-- information.
--
-- SECURITY: The security information from 'confirmNewUser' does not directly apply here: the
-- attacker needs to fulfil **all** three conditions mentioned above for a successful attack, not
-- only token secrecy.
confirmUserEmailChange :: ConfirmationToken -> Action e s ()
confirmUserEmailChange token = do
    expiryPeriod <- (>>. (Proxy :: Proxy '["email_change_expiration"])) <$> U.unsafeAction U.getConfig
    void . queryA $ T.confirmUserEmailChange expiryPeriod token


-- * service

allServiceIds :: Action e s [ServiceId]
allServiceIds = do
    taintMsg "allServiceIds" (RoleAdmin %% False)
    queryA T.allServiceIds

lookupService :: ServiceId -> Action e s (ServiceId, Service)
lookupService sid = queryA $ T.lookupService sid

addService ::
    UserId -> ServiceName -> ServiceDescription -> Action e s (ServiceId, ServiceKey)
addService owner name desc = do
    sid <- freshServiceId
    addServicePrim owner sid name desc

addServicePrim ::
    UserId -> ServiceId -> ServiceName -> ServiceDescription -> Action e s (ServiceId, ServiceKey)
addServicePrim owner sid name desc = do
    assertAuth (hasUserId owner <||> hasRole RoleAdmin)
    key <- freshServiceKey
    hashedKey <- U.unsafeAction $ U.hashServiceKey key
    queryA $ T.addService owner sid hashedKey name desc
    return (sid, key)

deleteService :: ServiceId -> Action e s ()
deleteService sid = do
    owner <- (^. serviceOwner) . snd <$> lookupService sid
    assertAuth (hasUserId owner <||> hasRole RoleAdmin)
    queryA $ T.deleteService sid

-- | Autocreate a service with a specific ID if it doesn't exist yet. Moreover, if no contexts
-- have been defined for the service yet, a default context with empty name is automatically
-- created.
--
-- This allows adding services to the config which will automatically spring into life if the
-- config is read.
autocreateServiceIfMissing :: UserId -> ServiceId -> Action e s ()
autocreateServiceIfMissing owner sid = do
    void (lookupService sid) `catchError`
        \case NoSuchService -> do
                loggerA DEBUG $ "autocreating service with ID " ++ show sid
                void $ addServicePrim owner sid "autocreated" "autocreated"
              e -> throwError e
    contexts <- contextsForService sid
    when (null contexts) . void $ addContext sid "" "default context" Nothing
    pure ()


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
lookupThentosSession :: ThentosSessionToken -> Action e s ThentosSession
lookupThentosSession tok = do
    session <- lookupThentosSession_ tok
    tryTaint (RoleAdmin \/ session ^. thSessAgent %% False)
        (return session)
        (\ (AnyLabelError _) -> throwError NoSuchThentosSession)

-- | Find 'ThentosSession' from token, without clearance check.
lookupThentosSession_ :: ThentosSessionToken -> Action e s ThentosSession
lookupThentosSession_ tok = snd <$> queryA (T.lookupThentosSession tok)

-- | Like 'lookupThentosSession', but does not throw an exception if thentos session does not exist
-- or is inaccessible, but returns 'False' instead.
existsThentosSession :: ThentosSessionToken -> Action e s Bool
existsThentosSession tok = (lookupThentosSession tok >> return True) `catchError`
    \case NoSuchThentosSession -> return False
          e                    -> throwError e

-- | Check user credentials and create a session for user.  Requires 'lookupConfirmedUser' and
-- '_startThentosSessionByAgent'.
startThentosSessionByUserId :: UserId -> UserPass -> Action e s ThentosSessionToken
startThentosSessionByUserId uid pass = do
    _ <- lookupUserCheckPassword_ (T.lookupConfirmedUser uid) pass
    startThentosSessionByAgent_ (UserA uid)

-- | Like 'startThentosSessionByUserId', but based on 'UserName' as key.
startThentosSessionByUserName ::
    UserName -> UserPass -> Action e s (UserId, ThentosSessionToken)
startThentosSessionByUserName name pass = do
    (uid, _) <- lookupUserCheckPassword_ (T.lookupConfirmedUserByName name) pass
    (,) uid <$> startThentosSessionByAgent_ (UserA uid)

startThentosSessionByUserEmail ::
    UserEmail -> UserPass -> Action e s (UserId, ThentosSessionToken)
startThentosSessionByUserEmail email pass = do
    (uid, _) <- lookupUserCheckPassword_ (T.lookupConfirmedUserByEmail email) pass
    (,) uid <$> startThentosSessionByAgent_ (UserA uid)

-- | Check service credentials and create a session for service.
startThentosSessionByServiceId ::
    ServiceId -> ServiceKey -> Action e s ThentosSessionToken
startThentosSessionByServiceId sid key = a `catchError` h
  where
    a = do
        (_, service) <- queryA (T.lookupService sid)
        unless (verifyKey key service) $ throwError BadCredentials
        startThentosSessionByAgent_ (ServiceA sid)

    h NoSuchService =
        throwError BadCredentials
    h e = throwError e

-- | Terminate 'ThentosSession'.  Does not require any label; being in possession of the session
-- token is enough authentication to terminate it.
endThentosSession :: ThentosSessionToken -> Action e s ()
endThentosSession = queryA . T.endThentosSession

-- | Check that a Thentos session exists, is not expired, and belongs to a user (rather than a
-- service). Returns information on the user if that's the case. Throws 'NoSuchThentosSession'
-- otherwise.
--
-- We assume that the ThentosSessionToken is a secret that nobody except the session owner can
-- know, therefore no special clearance is required.
validateThentosUserSession :: ThentosSessionToken -> Action e s (UserId, User)
validateThentosUserSession tok = do
    session <- lookupThentosSession_ tok
    case session ^. thSessAgent of
        UserA uid  -> queryA $ T.lookupConfirmedUser uid
        ServiceA _ -> throwError NoSuchThentosSession

-- | Open a session for any agent. Also promotes the access rights accordingly.
-- NOTE: This should only be called after verifying the agent's credentials
startThentosSessionByAgent_ :: Agent -> Action e s ThentosSessionToken
startThentosSessionByAgent_ agent = do
    tok <- freshSessionToken
    queryA $ T.startThentosSession tok agent defaultSessionTimeout
    U.extendClearanceOnAgent agent
    return tok

-- | For a thentos session, look up all service sessions and return their service names.  Requires
-- 'RoleAdmin', service, or user privs.
serviceNamesFromThentosSession :: ThentosSessionToken -> Action e s [ServiceName]
serviceNamesFromThentosSession tok = do
    assertAuth $ hasRole RoleAdmin
            <||> (hasAgent . (^. thSessAgent) =<< lookupThentosSession tok)
    queryA $ T.serviceNamesFromThentosSession tok


-- * service session

-- | Like 'lookupThentosSession', but for 'ServiceSession's.
lookupServiceSession :: ServiceSessionToken -> Action e s ServiceSession
lookupServiceSession tok = snd <$> queryA (T.lookupServiceSession tok)

-- | Like 'existsThentosSession', but for 'ServiceSession's.
existsServiceSession :: ServiceSessionToken -> Action e s Bool
existsServiceSession tok = (lookupServiceSession tok >> return True) `catchError`
    \case NoSuchServiceSession -> return False
          e                    -> throwError e

-- | (As soon as there is a good reason, we can export this.  just need to think about the label.)
thentosSessionAndUserIdByToken_ ::
    ThentosSessionToken -> Action e s (ThentosSession, UserId)
thentosSessionAndUserIdByToken_ tok = do
    session <- lookupThentosSession tok
    case session ^. thSessAgent of
        UserA uid -> return (session, uid)
        ServiceA sid -> throwError $ NeedUserA tok sid

-- | Register a user with a service.  Requires 'RoleAdmin' or user privs.
--
-- FIXME: We do not ask for any authorization from 'ServiceId' as of now.  It is enough to know a
-- 'ServiceId' to register with the resp. service.  This probably violates integrity of the view of
-- the service.  Fixing this may require credentials handling.  Before we do that, we should take a
-- better look at oauth.
addServiceRegistration :: ThentosSessionToken -> ServiceId -> Action e s ()
addServiceRegistration tok sid = do
    (_, uid) <- thentosSessionAndUserIdByToken_ tok
    queryA $ T.registerUserWithService uid sid newServiceAccount

-- | Undo registration of a user with a service.  Requires 'RoleAdmin' or user privs.
--
-- See FIXME in 'addServiceRegistration'.
dropServiceRegistration :: ThentosSessionToken -> ServiceId -> Action e s ()
dropServiceRegistration tok sid = do
    (_, uid) <- thentosSessionAndUserIdByToken_ tok
    guardWriteMsg "dropServiceRegistration"
        (RoleAdmin \/ UserA uid %% RoleAdmin /\ UserA uid)
    queryA $ T.unregisterUserFromService uid sid

-- | Login user running the current thentos session into service.  If user is not registered with
-- service, throw an error.
--
-- Inherits label and exception behavor from 'lookupThentosSession' and write-guards for thentos
-- session owner.
startServiceSession ::
    ThentosSessionToken -> ServiceId -> Action e s ServiceSessionToken
startServiceSession ttok sid = do
    _ <- lookupThentosSession ttok
    stok <- freshServiceSessionToken
    queryA $ T.startServiceSession ttok stok sid defaultSessionTimeout
    return stok

-- | Terminate service session. Throws NoSuchServiceSession if the user does not
-- own the session.
endServiceSession :: ServiceSessionToken -> Action e s ()
endServiceSession tok = queryA $ T.endServiceSession tok

-- | Inherits label from 'lookupServiceSession'.
getServiceSessionMetadata :: ServiceSessionToken -> Action e s ServiceSessionMetadata
getServiceSessionMetadata tok = (^. srvSessMetadata) <$> lookupServiceSession tok


-- * personas, contexts, and groups

-- | Add a new persona to the DB. A persona has a unique name and a user to which it belongs.
-- The 'PersonaId' is assigned by the DB. May throw 'NoSuchUser' or 'PersonaNameAlreadyExists'.
-- Only the user owning the persona or an admin may do this.
addPersona :: PersonaName -> UserId -> Maybe Uri -> Action e s Persona
addPersona name uid mExternalUrl = do
    assertAuth (hasUserId uid <||> hasRole RoleAdmin)
    queryA $ T.addPersona name uid mExternalUrl

-- | Delete a persona. Throw 'NoSuchPersona' if the persona does not exist in the DB.
-- Only the user owning the persona or an admin may do this.
deletePersona :: Persona -> Action e s ()
deletePersona persona = do
    assertAuth (hasUserId (persona ^. personaUid) <||> hasRole RoleAdmin)
    queryA . T.deletePersona $ persona ^. personaId

-- | Add a new context. The first argument identifies the service to which the context belongs.
-- May throw 'NoSuchService' or 'ContextNameAlreadyExists'.
-- Only the service or an admin may do this.
addContext :: ServiceId -> ContextName -> ContextDescription -> Maybe ProxyUri -> Action e s Context
addContext sid name desc mUrl = do
    assertAuth (hasServiceId sid <||> hasRole RoleAdmin)
    queryA $ T.addContext sid name desc mUrl

-- | Delete a context. Throw an error if the context does not exist in the DB.
-- Only the service owning the context or an admin may do this.
deleteContext :: Context -> Action e s ()
deleteContext context = do
    assertAuth (hasServiceId (context ^. contextService) <||> hasRole RoleAdmin)
    queryA $ T.deleteContext (context ^. contextService) (context ^. contextName)

-- | Connect a persona with a context. Throws an error if the persona is already registered for the
-- context or if the user has any *other* persona registered for the context
-- ('MultiplePersonasPerContext'). (As we currently allow only one persona per user and context.)
-- Throws 'NoSuchPersona' or 'NoSuchContext' if one of the arguments doesn't exist.
-- Only the user owning the persona or an admin may do this.
registerPersonaWithContext :: Persona -> ServiceId -> ContextName -> Action e s ()
registerPersonaWithContext persona sid cname = do
    assertAuth (hasUserId (persona ^. personaUid) <||> hasRole RoleAdmin)
    queryA $ T.registerPersonaWithContext persona sid cname

-- | Unregister a persona from accessing a context. No-op if the persona was not registered for the
-- context. Only the user owning the persona or an admin may do this.
unregisterPersonaFromContext :: Persona -> ServiceId -> ContextName -> Action e s ()
unregisterPersonaFromContext persona sid cname = do
    assertAuth (hasUserId (persona ^. personaUid) <||> hasRole RoleAdmin)
    queryA $ T.unregisterPersonaFromContext (persona ^. personaId) sid cname

-- | Find the persona that a user wants to use for a context (if any).
-- Only the user owning the persona or an admin may do this.
findPersona :: UserId -> ServiceId -> ContextName -> Action e s (Maybe Persona)
findPersona uid sid cname = do
    assertAuth (hasUserId uid <||> hasRole RoleAdmin)
    queryA $ T.findPersona uid sid cname

-- | List all contexts owned by a service. Anybody may do this.
contextsForService :: ServiceId -> Action e s [Context]
contextsForService sid = queryA $ T.contextsForService sid

-- | Add a persona to a group. If the persona is already a member of the group, do nothing.
-- Only a GroupAdmin may do this.
addPersonaToGroup :: PersonaId -> ServiceGroup -> Action e s ()
addPersonaToGroup pid group = do
    assertAuth $ hasRole RoleGroupAdmin
    queryA $ T.addPersonaToGroup pid group

-- | Remove a persona from a group. If the persona is not a member of the group, do nothing.
-- Only a GroupAdmin may do this.
removePersonaFromGroup :: PersonaId -> ServiceGroup -> Action e s ()
removePersonaFromGroup pid group = do
    assertAuth $ hasRole RoleGroupAdmin
    queryA $ T.removePersonaFromGroup pid group

-- | Add a group (subgroup) to another group (supergroup) so that all members of subgroup will also
-- be considered members of supergroup. If subgroup is already a direct member of supergroup, do
-- nothing. Throws 'GroupMembershipLoop' if adding the relation would cause a loop.
-- Only a GroupAdmin may do this.
addGroupToGroup :: ServiceGroup -> ServiceGroup -> Action e s ()
addGroupToGroup subgroup supergroup = do
    assertAuth $ hasRole RoleGroupAdmin
    queryA $ T.addGroupToGroup subgroup supergroup

-- | Remove a group (subgroup) from another group (supergroup). If subgroup is not a direct
-- member of supergroup, do nothing. Only a GroupAdmin may do this.
removeGroupFromGroup :: ServiceGroup -> ServiceGroup -> Action e s ()
removeGroupFromGroup subgroup supergroup = do
    assertAuth $ hasRole RoleGroupAdmin
    queryA $ T.removeGroupFromGroup subgroup supergroup

-- | List all groups a persona belongs to, directly or indirectly. If p is a member of g1,
-- g1 is a member of g2, and g2 is a member of g3, [g1, g2, g3] will be returned.
-- Only the user owning the persona or a GroupAdmin may do this.
personaGroups :: Persona -> Action e s [ServiceGroup]
personaGroups persona = do
    assertAuth $ hasUserId (persona ^. personaUid) <||> hasRole RoleGroupAdmin
    queryA $ T.personaGroups (persona ^. personaId)


-- * agents and roles

assignRole :: Agent -> Group -> Action e s ()
assignRole agent role = do
    guardWriteMsg "assignRole" (RoleAdmin %% RoleAdmin)
    queryA $ T.assignRole agent role

unassignRole :: Agent -> Group -> Action e s ()
unassignRole agent role = do
    guardWriteMsg "unassignRole" (RoleAdmin %% RoleAdmin)
    queryA $ T.unassignRole agent role

agentRoles :: Agent -> Action e s [Group]
agentRoles agent = do
    taintMsg "agentRoles" (RoleAdmin \/ agent %% RoleAdmin /\ agent)
    queryA (T.agentRoles agent)


-- * Sybil attack prevention

-- | Generate a captcha. Returns a pair of 'CaptchaId' and the binary image data in PNG format.
-- The correct solution to the captcha is stored in the DB. Does not require any privileges.
makeCaptcha :: Action e s (CaptchaId, ImageData)
makeCaptcha = do
    cid    <- freshCaptchaId
    random <- freshRandom20
    (imgdata, solution) <- U.unsafeLiftIO $ Sybil.generateCaptcha random
    queryA $ T.storeCaptcha cid solution
    pure (cid, imgdata)

-- | Argument must be an espeak voice installed on the server system.  Try "en", "de", "fi", "ru" or
-- call `espeak --voices` for a complete list.
makeAudioCaptcha :: String -> Action e s (CaptchaId, SBS)
makeAudioCaptcha eSpeakVoice = do
    cid    <- freshCaptchaId
    random <- freshRandom20
    (wav, solution) <- Sybil.generateAudioCaptcha eSpeakVoice random
    queryA $ T.storeCaptcha cid solution
    pure (cid, wav)

-- | Submit a solution to a captcha, returning whether or not the solution is correct.
-- If the solution is wrong, delete captcha to prevent multiple guesses.
-- Throws 'NoSuchCaptchaId' if the given 'CaptchaId' doesn't exist in the DB (either because it
-- never did or because it was deleted). Does not require any privileges.
solveCaptcha :: CaptchaId -> ST -> Action e s Bool
solveCaptcha cid solution = do
    solutionCorrect <- queryA $ T.solveCaptcha cid solution
    unless solutionCorrect $ deleteCaptcha cid
    return solutionCorrect

-- | Delete a captcha and its solution from the DB. Throws 'NoSuchCaptchaId' if the given
-- 'CaptchaId' doesn't exist in the DB (either because it never did or because it was deleted due
-- to garbage collection or a prior call to this action). Does not require any privileges.
deleteCaptcha :: CaptchaId -> Action e s ()
deleteCaptcha = queryA . T.deleteCaptcha


-- * garbage collection

collectGarbage :: Exception (ActionError e) => Action e s ()
collectGarbage = do
    loggerA DEBUG "starting garbage collection."
    guardWriteMsg "collectGarbage" (RoleAdmin %% RoleAdmin)

    queryA T.garbageCollectThentosSessions
    queryA T.garbageCollectServiceSessions

    config <- U.unsafeAction U.getConfig
    let userExpiry     = config >>. (Proxy :: Proxy '["user_reg_expiration"])
        passwordExpiry = config >>. (Proxy :: Proxy '["pw_reset_expiration"])
        emailExpiry    = config >>. (Proxy :: Proxy '["email_change_expiration"])
        captchaExpiry  = config >>. (Proxy :: Proxy '["captcha_expiration"])
    queryA $ T.garbageCollectUnconfirmedUsers userExpiry
    queryA $ T.garbageCollectEmailChangeTokens emailExpiry
    queryA $ T.garbageCollectPasswordResetTokens passwordExpiry
    queryA $ T.garbageCollectCaptchas captchaExpiry

    loggerA DEBUG "garbage collection complete!"
