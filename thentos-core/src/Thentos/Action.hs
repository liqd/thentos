{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ViewPatterns         #-}

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
    , sendPasswordResetMail
    , resetPassword
    , resetPasswordAndLogin
    , changePassword
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

    , sendEmail

    , assignGroup
    , unassignGroup
    , agentGroups

    , makeCaptcha
    , makeAudioCaptcha
    , solveCaptcha
    , deleteCaptcha

    , collectGarbage
    )
where

import Thentos.Prelude
import Data.Configifier ((>>.), Tagged(Tagged))
import Text.Hastache.Context (mkStrContext)
import Text.Hastache (MuType(MuVariable))

import qualified Codec.Binary.Base64 as Base64
import qualified Data.Text as ST

import Thentos.Action.Core
import Thentos.Action.Types
import Thentos.Action.TCB
import Thentos.Action.SimpleAuth
import Thentos.Config
import Thentos.Transaction.Core (ThentosQuery)
import Thentos.Types
import Thentos.Util (verifyUserPass, verifyServiceKey)

import qualified Thentos.Action.Unsafe as U
import qualified Thentos.Sybil as Sybil
import qualified Thentos.Transaction as T


queryA :: MonadQuery e v m => ThentosQuery e a -> m a
queryA = U.query

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
freshRandomName :: MonadRandom m => m ST
freshRandomName = ST.replace "/" "_" . cs . Base64.encode <$> getRandomBytes 18

freshConfirmationToken :: MonadRandom m => m ConfirmationToken
freshConfirmationToken = ConfirmationToken <$> freshRandomName

-- | Generate 20 bytes of random data.
-- For comparison: an UUID has 16 bytes, so that should be enough for all practical purposes.
freshRandom20 :: MonadRandom m => m Random20
freshRandom20 = do
    bytes <- getRandomBytes 20
    maybe (error "freshRandom20: internal error") pure $ mkRandom20 bytes

freshPasswordResetToken :: MonadRandom m => m PasswordResetToken
freshPasswordResetToken = PasswordResetToken <$> freshRandomName

freshServiceId :: MonadRandom m => m ServiceId
freshServiceId = ServiceId <$> freshRandomName

freshServiceKey :: MonadRandom m => m ServiceKey
freshServiceKey = ServiceKey <$> freshRandomName

freshSessionToken :: MonadRandom m => m ThentosSessionToken
freshSessionToken = ThentosSessionToken <$> freshRandomName

freshServiceSessionToken :: MonadRandom m => m ServiceSessionToken
freshServiceSessionToken = ServiceSessionToken <$> freshRandomName

freshCaptchaId :: MonadRandom m => m CaptchaId
freshCaptchaId = CaptchaId <$> freshRandomName


-- * user

-- | Return a user with its id.  Requires or privileges of admin or the user that is looked up.  If
-- no user is found or access is not granted, throw 'NoSuchUser'.  See 'lookupUserCheckPassword_' for
-- user lookup prior to authentication.
lookupConfirmedUser :: MonadQuery e v m => UserId -> m (UserId, User)
lookupConfirmedUser = lookupUser_ . T.lookupConfirmedUser

lookupUser_ :: MonadQuery e v m => ThentosQuery e (UserId, User) -> m (UserId, User)
lookupUser_ transaction = do
    val@(uid, _) <- queryA transaction
    tryTaint (GroupAdmin \/ UserA uid %% False)
        (return val)
        (\ (AnyLabelError _) -> throwError NoSuchUser)

-- | Like 'lookupConfirmedUser', but based on 'UserName'.
lookupConfirmedUserByName :: MonadQuery e v m => UserName -> m (UserId, User)
lookupConfirmedUserByName = lookupUser_ . T.lookupConfirmedUserByName

-- | Like 'lookupConfirmedUser', but based on 'UserEmail'.
lookupConfirmedUserByEmail :: MonadQuery e v m => UserEmail -> m (UserId, User)
lookupConfirmedUserByEmail = lookupUser_ . T.lookupConfirmedUserByEmail

-- | Add a user based on its form data.  Requires 'GroupAdmin'.  For creating users with e-mail
-- verification, see 'addUnconfirmedUser', 'confirmNewUser'.
addUser :: MonadQuery e v m => UserFormData -> m UserId
addUser userData = do
    guardWriteMsg "addUser" (GroupAdmin %% GroupAdmin)
    makeUserFromFormData userData >>= queryA . T.addUser

-- | Delete user.  Requires or privileges of admin or the user that is looked up.  If no user is
-- found or access is not granted, throw 'NoSuchUser'.
deleteUser :: MonadQuery e v m => UserId -> m ()
deleteUser uid = do
    guardWriteMsg "deleteUser" (GroupAdmin \/ UserA uid %% GroupAdmin /\ UserA uid)
    queryA $ T.deleteUser uid


-- ** email confirmation

-- | Initiate email-verified user creation.  Does not require any privileges.
-- This also sends an email containing an activation link on which the user must click.
-- See also: 'confirmNewUser'.
addUnconfirmedUser :: MonadAction e v m => UserFormData -> m ()
addUnconfirmedUser userData = do
    passwordAcceptable $ udPassword userData
    tok  <- freshConfirmationToken
    user <- makeUserFromFormData userData
    let happy = do
            void . queryA $ T.addUnconfirmedUser tok user
            sendUserConfirmationMail userData tok
    happy
        `catchError`
            \case UserEmailAlreadyExists -> sendUserExistsMail (udEmail userData)
                  e                      -> throwError e

-- | Helper action: Check that the new or changed password of a user is acceptable, throwing an
-- error otherwise. Currently we only check that the password is not too short.
passwordAcceptable :: MonadThentosError e m => UserPass -> m ()
passwordAcceptable (UserPass pass) =
    if ST.length pass < minPasswordLength then throwError PasswordTooShort else pure ()

-- | Helper action: Send a confirmation mail to a newly registered user.
sendUserConfirmationMail :: MonadQuery e v m => UserFormData -> ConfirmationToken -> m ()
sendUserConfirmationMail user (ConfirmationToken confToken) = do
    cfg <- getConfig
    let subject = cfg >>. (Proxy :: Proxy '["email_templates", "account_verification", "subject"])
        bodyTemplate = cfg >>. (Proxy :: Proxy '["email_templates", "account_verification", "body"])
        feHttp = case cfg >>. (Proxy :: Proxy '["frontend"]) of
            Nothing -> error "sendUserConfirmationMail: frontend not configured!"
            Just v -> Tagged v
        context "user_name"      = MuVariable . fromUserName $ udName user
        context "activation_url" = MuVariable $ exposeUrl feHttp <//> "/activate/" <//> confToken
        context x                = error $ "sendUserConfirmationMail: no such context: " ++ x
    body <- renderTextTemplate bodyTemplate (mkStrContext context)
    sendMail (Just $ udName user) (udEmail user) subject (cs body) Nothing

-- | Helper action: Send a confirmation mail to a newly registered user.
sendUserExistsMail :: MonadQuery e v m => UserEmail -> m ()
sendUserExistsMail email = do
    cfg <- getConfig
    let subject = cfg >>. (Proxy :: Proxy '["email_templates", "user_exists", "subject"])
        body    = cfg >>. (Proxy :: Proxy '["email_templates", "user_exists", "body"])
    sendMail Nothing email subject body Nothing

-- | Initiate email-verified user creation.  Does not require any privileges, but the user must
-- have correctly solved a captcha to prove that they are human.  After the new user has been
-- created, the captcha is deleted to prevent an attacker from creating multiple users with
-- the same captcha solution.  If user creation fails (e.g. because of a duplicate user name), the
-- captcha remains in the DB to allow another attempt.  See also: 'makeCaptcha', 'confirmNewUser'.
addUnconfirmedUserWithCaptcha :: MonadAction e v m => UserCreationRequest -> m ()
addUnconfirmedUserWithCaptcha ucr = do
    captchaCorrect <- solveCaptcha (csId $ ucCaptcha ucr) (csSolution $ ucCaptcha ucr)
    let captchaAttempt = if captchaCorrect then CaptchaCorrect else CaptchaIncorrect
    logSignupAttempt (udName $ ucUser ucr) (udEmail $ ucUser ucr) captchaAttempt
    unless captchaCorrect $
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
confirmNewUser :: MonadAction e v m => ConfirmationToken -> m (UserId, ThentosSessionToken)
confirmNewUser token = do
    expiryPeriod <- getConfigField (Proxy :: Proxy '["user_reg_expiration"])
    uid <- queryA $ T.finishUserRegistration expiryPeriod token
    sessionToken <- startThentosSessionByAgent_ (UserA uid)
    return (uid, sessionToken)


-- ** password reset

-- | Initiate password reset with email confirmation.  No authentication required, obviously.
--
-- NOTE: This leaks existence info on email addresses.  But if we do not want to throw 'NoSuchUser'
-- here, we run into other issues:
--
-- - If we send mail to people not in your DB ("somebody entered your address in your password reset
--   field, but you're not registered. If you want to register, please do so at X. If not, sorry and
--   just ignore this email.") we could annoy them. It's not so bad if that happens just once, but
--   what if somebody triggers a password reset for Angela Merkel's email address every two seconds?
--
-- - If we just silently ignore the error, it would be a bad UI experience for people who thought
--   they registered with one address but actually used another one -- since we don't tell them that
--   that the address is unkown and don't take any further action, it's very hard for them to figure
--   out why they never receive the expected reset link.
addPasswordResetToken :: MonadAction e v m => UserEmail -> m (User, PasswordResetToken)
addPasswordResetToken email = do
    tok <- freshPasswordResetToken
    user <- queryA $ T.addPasswordResetToken email tok
    return (user, tok)

-- | Create a password reset token and send the link by email to the user.
-- The first argument can be used to modify the reset link sent per email.
-- No authentication required, obviously.
sendPasswordResetMail :: MonadAction e v m => Maybe ST -> UserEmail -> m ()
sendPasswordResetMail beforeToken email = do
    (user, PasswordResetToken tok) <- addPasswordResetToken email
    cfg <- getConfig
    let subject      = cfg >>. (Proxy :: Proxy '["email_templates", "password_reset", "subject"])
        bodyTemplate = cfg >>. (Proxy :: Proxy '["email_templates", "password_reset", "body"])
        feHttp       = case cfg >>. (Proxy :: Proxy '["frontend"]) of
            Nothing -> error "sendPasswordResetMail: frontend not configured!"
            Just v -> Tagged v
        fullTok      = fromMaybe "" beforeToken <> tok
        context "user_name" = MuVariable . fromUserName $ user ^. userName
        context "reset_url" = MuVariable $ exposeUrl feHttp <//> "/password_reset/" <//> fullTok
        context x           = error $ "sendPasswordResetMail: no such context: " ++ x
    body <- renderTextTemplate bodyTemplate (mkStrContext context)
    sendMail (Just $ user ^. userName) (user ^. userEmail) subject (cs body) Nothing

-- | Finish password reset with email confirmation. Also confirms the user's email address if
-- that hasn't happened before. Returns the ID of the updated user.
--
-- SECURITY: See 'confirmNewUser'.
resetPassword :: MonadQuery e v m => PasswordResetToken -> UserPass -> m UserId
resetPassword token password = do
    passwordAcceptable password
    expiryPeriod <- (>>. (Proxy :: Proxy '["pw_reset_expiration"])) <$> getConfig
    hashedPassword <- hashUserPass password
    queryA $ T.resetPassword expiryPeriod token hashedPassword


-- | Finish password reset with email confirmation and open a new ThentosSession for the user.
--
-- SECURITY: See 'confirmNewUser'.
resetPasswordAndLogin :: MonadAction e v m => PasswordResetToken -> UserPass -> m ThentosSessionToken
resetPasswordAndLogin token password = do
    uid <- resetPassword token password
    startThentosSessionByAgent_ (UserA uid)


-- ** login

-- | Find user running the action, confirm the password, and return the user or crash.
-- 'NoSuchUser' is translated into 'BadCredentials'.
-- NOTE: This should not be exported from this module, as it allows access to
-- the user map without any clearance.
lookupUserCheckPassword_ ::
    MonadQuery e v m => ThentosQuery e (UserId, User) -> UserPass -> m (UserId, User)
lookupUserCheckPassword_ transaction password = a `catchError` h
  where
    a = do
        (uid, user) <- queryA transaction
        if verifyUserPass password user
            then return (uid, user)
            else throwError BadCredentials

    h NoSuchUser = throwError BadCredentials
    h e          = throwError e


-- ** change user data

-- | Authenticate user against old password, and then change password to new password.
--
-- LIO policy: In addition to the old password as proof of authority, this function requires the
-- user to change the password to be logged in (or admin privs).
changePassword :: MonadQuery e v m => UserId -> UserPass -> UserPass -> m ()
changePassword uid old new = do
    passwordAcceptable new
    _ <- lookupUserCheckPassword_ (T.lookupAnyUser uid) old
    hashedPw <- hashUserPass new
    guardWriteMsg "changePassword" (GroupAdmin \/ UserA uid %% GroupAdmin /\ UserA uid)
    queryA $ T.changePassword uid hashedPw

-- | Initiate email change by creating and storing a token and sending it out by email to the old
-- address of the user.  This requires 'GroupAdmin' or privs of email address owner, but the address
-- is only changed after a call to 'confirmUserEmailChange' with the correct token.
requestUserEmailChange ::
    MonadAction e v m => UserId -> UserEmail -> (ConfirmationToken -> ST) -> m ()
requestUserEmailChange uid newEmail callbackUrlBuilder = do
    guardWriteMsg "requestUserEmailChange" (GroupAdmin \/ UserA uid %% GroupAdmin /\ UserA uid)

    tok <- freshConfirmationToken
    queryA $ T.addUserEmailChangeRequest uid newEmail tok

    let message = "Please go to " <> callbackUrlBuilder tok <> " to confirm your change of email address."
        subject = "Thentos email address change"
    sendMail Nothing newEmail subject message Nothing

-- | Look up the given confirmation token and updates the user's email address iff 1) the token
-- exists, 2) the token belongs to the user currently logged in, and 3) the token has not
-- expired. If any of these conditions don't apply, throw 'NoSuchToken' to avoid leaking
-- information.
--
-- SECURITY: The security information from 'confirmNewUser' does not directly apply here: the
-- attacker needs to fulfil **all** three conditions mentioned above for a successful attack, not
-- only token secrecy.
confirmUserEmailChange :: MonadQuery e v m => ConfirmationToken -> m ()
confirmUserEmailChange token = do
    expiryPeriod <- getConfigField (Proxy :: Proxy '["email_change_expiration"])
    void . queryA $ T.confirmUserEmailChange expiryPeriod token


-- * service

allServiceIds :: MonadQuery e v m => m [ServiceId]
allServiceIds = do
    taintMsg "allServiceIds" (GroupAdmin %% False)
    queryA T.allServiceIds

lookupService :: MonadQuery e v m => ServiceId -> m (ServiceId, Service)
lookupService sid = queryA $ T.lookupService sid

addService :: MonadAction e v m =>
    UserId -> ServiceName -> ServiceDescription -> m (ServiceId, ServiceKey)
addService owner name desc = do
    sid <- freshServiceId
    addServicePrim owner sid name desc

addServicePrim :: MonadAction e v m =>
    UserId -> ServiceId -> ServiceName -> ServiceDescription -> m (ServiceId, ServiceKey)
addServicePrim owner sid name desc = do
    assertAuth (hasUserId owner <||> hasGroup GroupAdmin)
    key <- freshServiceKey
    hashedKey <- hashServiceKey key
    queryA $ T.addService owner sid hashedKey name desc
    return (sid, key)

deleteService :: MonadQuery e v m => ServiceId -> m ()
deleteService sid = do
    owner <- (^. serviceOwner) . snd <$> lookupService sid
    assertAuth (hasUserId owner <||> hasGroup GroupAdmin)
    queryA $ T.deleteService sid

-- | Autocreate a service with a specific ID if it doesn't exist yet. Moreover, if no contexts
-- have been defined for the service yet, a default context with empty name is automatically
-- created.
--
-- This allows adding services to the config which will automatically spring into life if the
-- config is read.
autocreateServiceIfMissing :: MonadAction e v m => UserId -> ServiceId -> m ()
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
lookupThentosSession :: MonadQuery e v m => ThentosSessionToken -> m ThentosSession
lookupThentosSession tok = do
    session <- lookupThentosSession_ tok
    tryTaint (GroupAdmin \/ session ^. thSessAgent %% False)
        (return session)
        (\ (AnyLabelError _) -> throwError NoSuchThentosSession)

-- | Find 'ThentosSession' from token, without clearance check.
lookupThentosSession_ :: MonadQuery e v m => ThentosSessionToken -> m ThentosSession
lookupThentosSession_ tok = snd <$> queryA (T.lookupThentosSession tok)

-- | Like 'lookupThentosSession', but does not throw an exception if thentos session does not exist
-- or is inaccessible, but returns 'False' instead.
existsThentosSession :: MonadQuery e v m => ThentosSessionToken -> m Bool
existsThentosSession tok = (lookupThentosSession tok >> return True) `catchError`
    \case NoSuchThentosSession -> return False
          e                    -> throwError e

-- | Check user credentials and create a session for user.  Requires 'lookupConfirmedUser' and
-- '_startThentosSessionByAgent'.
startThentosSessionByUserId :: MonadAction e v m => UserId -> UserPass -> m ThentosSessionToken
startThentosSessionByUserId uid pass = do
    _ <- lookupUserCheckPassword_ (T.lookupConfirmedUser uid) pass
    startThentosSessionByAgent_ (UserA uid)

-- | Like 'startThentosSessionByUserId', but based on 'UserName' as key.
startThentosSessionByUserName :: MonadAction e v m =>
    UserName -> UserPass -> m (UserId, ThentosSessionToken)
startThentosSessionByUserName name pass = do
    (uid, _) <- lookupUserCheckPassword_ (T.lookupConfirmedUserByName name) pass
    (,) uid <$> startThentosSessionByAgent_ (UserA uid)

startThentosSessionByUserEmail :: MonadAction e v m =>
    UserEmail -> UserPass -> m (UserId, ThentosSessionToken)
startThentosSessionByUserEmail email pass = do
    (uid, _) <- lookupUserCheckPassword_ (T.lookupConfirmedUserByEmail email) pass
    (,) uid <$> startThentosSessionByAgent_ (UserA uid)

-- | Check service credentials and create a session for service.
startThentosSessionByServiceId :: MonadAction e v m =>
    ServiceId -> ServiceKey -> m ThentosSessionToken
startThentosSessionByServiceId sid key = a `catchError` h
  where
    a = do
        (_, service) <- queryA (T.lookupService sid)
        unless (verifyServiceKey key service) $ throwError BadCredentials
        startThentosSessionByAgent_ (ServiceA sid)

    h NoSuchService =
        throwError BadCredentials
    h e = throwError e

-- | Terminate 'ThentosSession'.  Does not require any label; being in possession of the session
-- token is enough authentication to terminate it.
endThentosSession :: MonadQuery e v m => ThentosSessionToken -> m ()
endThentosSession = queryA . T.endThentosSession

-- | Check that a Thentos session exists, is not expired, and belongs to a user (rather than a
-- service). Returns information on the user if that's the case. Throws 'NoSuchThentosSession'
-- otherwise.
--
-- We assume that the ThentosSessionToken is a secret that nobody except the session owner can
-- know, therefore no special clearance is required.
validateThentosUserSession :: MonadQuery e v m => ThentosSessionToken -> m (UserId, User)
validateThentosUserSession tok = do
    session <- lookupThentosSession_ tok
    case session ^. thSessAgent of
        UserA uid  -> queryA $ T.lookupConfirmedUser uid
        ServiceA _ -> throwError NoSuchThentosSession

-- | Open a session for any agent. Also promotes the access rights accordingly.
-- NOTE: This should only be called after verifying the agent's credentials
startThentosSessionByAgent_ :: MonadAction e v m => Agent -> m ThentosSessionToken
startThentosSessionByAgent_ agent = do
    tok <- freshSessionToken
    queryA $ T.startThentosSession tok agent defaultSessionTimeout
    U.extendClearanceOnAgent agent
    return tok

-- | For a thentos session, look up all service sessions and return their service names.  Requires
-- 'GroupAdmin', service, or user privs.
serviceNamesFromThentosSession :: MonadQuery e v m => ThentosSessionToken -> m [ServiceName]
serviceNamesFromThentosSession tok = do
    assertAuth $ hasGroup GroupAdmin
            <||> (hasAgent . (^. thSessAgent) =<< lookupThentosSession tok)
    queryA $ T.serviceNamesFromThentosSession tok


-- * service session

-- | Like 'lookupThentosSession', but for 'ServiceSession's.
lookupServiceSession :: MonadQuery e v m => ServiceSessionToken -> m ServiceSession
lookupServiceSession tok = snd <$> queryA (T.lookupServiceSession tok)

-- | Like 'existsThentosSession', but for 'ServiceSession's.
existsServiceSession :: MonadQuery e v m => ServiceSessionToken -> m Bool
existsServiceSession tok = (lookupServiceSession tok >> return True) `catchError`
    \case NoSuchServiceSession -> return False
          e                    -> throwError e

-- | (As soon as there is a good reason, we can export this.  just need to think about the label.)
thentosSessionAndUserIdByToken_ ::
    MonadQuery e v m => ThentosSessionToken -> m (ThentosSession, UserId)
thentosSessionAndUserIdByToken_ tok = do
    session <- lookupThentosSession tok
    case session ^. thSessAgent of
        UserA uid -> return (session, uid)
        ServiceA sid -> throwError $ NeedUserA tok sid

-- | Register a user with a service.  Requires 'GroupAdmin' or user privs.
--
-- FIXME: We do not ask for any authorization from 'ServiceId' as of now.  It is enough to know a
-- 'ServiceId' to register with the resp. service.  This probably violates integrity of the view of
-- the service.  Fixing this may require credentials handling.  Before we do that, we should take a
-- better look at oauth.
addServiceRegistration :: MonadQuery e v m => ThentosSessionToken -> ServiceId -> m ()
addServiceRegistration tok sid = do
    (_, uid) <- thentosSessionAndUserIdByToken_ tok
    queryA $ T.registerUserWithService uid sid newServiceAccount

-- | Undo registration of a user with a service.  Requires 'GroupAdmin' or user privs.
--
-- See FIXME in 'addServiceRegistration'.
dropServiceRegistration :: MonadQuery e v m => ThentosSessionToken -> ServiceId -> m ()
dropServiceRegistration tok sid = do
    (_, uid) <- thentosSessionAndUserIdByToken_ tok
    guardWriteMsg "dropServiceRegistration"
        (GroupAdmin \/ UserA uid %% GroupAdmin /\ UserA uid)
    queryA $ T.unregisterUserFromService uid sid

-- | Login user running the current thentos session into service.  If user is not registered with
-- service, throw an error.
--
-- Inherits label and exception behavor from 'lookupThentosSession' and write-guards for thentos
-- session owner.
startServiceSession :: MonadAction e v m => ThentosSessionToken -> ServiceId -> m ServiceSessionToken
startServiceSession ttok sid = do
    _ <- lookupThentosSession ttok
    stok <- freshServiceSessionToken
    queryA $ T.startServiceSession ttok stok sid defaultSessionTimeout
    return stok

-- | Terminate service session. Throws NoSuchServiceSession if the user does not
-- own the session.
endServiceSession :: MonadQuery e v m => ServiceSessionToken -> m ()
endServiceSession tok = queryA $ T.endServiceSession tok

-- | Inherits label from 'lookupServiceSession'.
getServiceSessionMetadata :: MonadQuery e v m => ServiceSessionToken -> m ServiceSessionMetadata
getServiceSessionMetadata tok = (^. srvSessMetadata) <$> lookupServiceSession tok


-- * send emails

-- | Send an email to one or many recipients.  See docs/messaging.md for more details.  Only a
-- privileged IP is allowed to use this endpoint.
{- FIXME Templating issues:
    * Privacy:
        In a way the purpose of having thentos sending emails is to avoid
        the privacy leakage that would be caused if we would instead provide
        a method of access the email addresses.
        However the templating is also a way to leak information in particular
        when the variable is used in a URL calling home (.e.g. http://example.com/{{name}}).
        So if there is any templating it should be about what the service already knows.
    * Duplicates:
        In case of duplicates (see docs/messaging.md) what should be the templates variables?
    * Mixed recipients:
        When sending directly to an email address on has no additional information for templating,
        while when sending to a persona one has access to some information for templating.
-}
sendEmail :: MonadQuery e v m => SendEmailRequest -> m ()
sendEmail req = do
    void hasPrivilegedIP
    let recipients = req ^. emailRecipients
        personaEmail uri = queryA $ do
            p <- T.lookupPersonaByUri uri
            (_, u) <- T.lookupConfirmedUser (p ^. personaUid)
            return $ u ^. userEmail
    personaEmails <- mapM personaEmail (recipients ^. erPersonas)
    let emails = nub $ recipients ^. erEmails ++ personaEmails
    for_ emails $ \email ->
        sendMail Nothing email (req ^. emailSubject) (req ^. emailBody) (req ^. emailHtml)


-- * personas, contexts, and groups

-- | Add a new persona to the DB. A persona has a unique name and a user to which it belongs.
-- The 'PersonaId' is assigned by the DB. May throw 'NoSuchUser' or 'PersonaNameAlreadyExists'.
-- Only the user owning the persona or an admin may do this.
addPersona :: MonadQuery e v m => PersonaName -> UserId -> Maybe Uri -> m Persona
addPersona name uid mExternalUrl = do
    assertAuth (hasUserId uid <||> hasGroup GroupAdmin)
    queryA $ T.addPersona name uid mExternalUrl

-- | Delete a persona. Throw 'NoSuchPersona' if the persona does not exist in the DB.
-- Only the user owning the persona or an admin may do this.
deletePersona :: MonadQuery e v m => Persona -> m ()
deletePersona persona = do
    assertAuth (hasUserId (persona ^. personaUid) <||> hasGroup GroupAdmin)
    queryA . T.deletePersona $ persona ^. personaId

-- | Add a new context. The first argument identifies the service to which the context belongs.
-- May throw 'NoSuchService' or 'ContextNameAlreadyExists'.
-- Only the service or an admin may do this.
addContext :: MonadQuery e v m => ServiceId -> ContextName -> ContextDescription -> Maybe ProxyUri -> m Context
addContext sid name desc mUrl = do
    assertAuth (hasServiceId sid <||> hasGroup GroupAdmin)
    queryA $ T.addContext sid name desc mUrl

-- | Delete a context. Throw an error if the context does not exist in the DB.
-- Only the service owning the context or an admin may do this.
deleteContext :: MonadQuery e v m => Context -> m ()
deleteContext context = do
    assertAuth (hasServiceId (context ^. contextService) <||> hasGroup GroupAdmin)
    queryA $ T.deleteContext (context ^. contextService) (context ^. contextName)

-- | Connect a persona with a context. Throws an error if the persona is already registered for the
-- context or if the user has any *other* persona registered for the context
-- ('MultiplePersonasPerContext'). (As we currently allow only one persona per user and context.)
-- Throws 'NoSuchPersona' or 'NoSuchContext' if one of the arguments doesn't exist.
-- Only the user owning the persona or an admin may do this.
registerPersonaWithContext :: MonadQuery e v m => Persona -> ServiceId -> ContextName -> m ()
registerPersonaWithContext persona sid cname = do
    assertAuth (hasUserId (persona ^. personaUid) <||> hasGroup GroupAdmin)
    queryA $ T.registerPersonaWithContext persona sid cname

-- | Unregister a persona from accessing a context. No-op if the persona was not registered for the
-- context. Only the user owning the persona or an admin may do this.
unregisterPersonaFromContext :: MonadQuery e v m => Persona -> ServiceId -> ContextName -> m ()
unregisterPersonaFromContext persona sid cname = do
    assertAuth (hasUserId (persona ^. personaUid) <||> hasGroup GroupAdmin)
    queryA $ T.unregisterPersonaFromContext (persona ^. personaId) sid cname

-- | Find the persona that a user wants to use for a context (if any).
-- Only the user owning the persona or an admin may do this.
findPersona :: MonadQuery e v m => UserId -> ServiceId -> ContextName -> m (Maybe Persona)
findPersona uid sid cname = do
    assertAuth (hasUserId uid <||> hasGroup GroupAdmin)
    queryA $ T.findPersona uid sid cname

-- | List all contexts owned by a service. Anybody may do this.
contextsForService :: MonadQuery e v m => ServiceId -> m [Context]
contextsForService sid = queryA $ T.contextsForService sid

-- | Add a persona to a group. If the persona is already a member of the group, do nothing.
-- Only a GroupAdmin may do this.
addPersonaToGroup :: MonadQuery e v m => PersonaId -> ServiceGroup -> m ()
addPersonaToGroup pid group = do
    assertAuth $ hasGroup GroupGroupAdmin
    queryA $ T.addPersonaToGroup pid group

-- | Remove a persona from a group. If the persona is not a member of the group, do nothing.
-- Only a GroupAdmin may do this.
removePersonaFromGroup :: MonadQuery e v m => PersonaId -> ServiceGroup -> m ()
removePersonaFromGroup pid group = do
    assertAuth $ hasGroup GroupGroupAdmin
    queryA $ T.removePersonaFromGroup pid group

-- | Add a group (subgroup) to another group (supergroup) so that all members of subgroup will also
-- be considered members of supergroup. If subgroup is already a direct member of supergroup, do
-- nothing. Throws 'GroupMembershipLoop' if adding the relation would cause a loop.
-- Only a GroupAdmin may do this.
addGroupToGroup :: MonadQuery e v m => ServiceGroup -> ServiceGroup -> m ()
addGroupToGroup subgroup supergroup = do
    assertAuth $ hasGroup GroupGroupAdmin
    queryA $ T.addGroupToGroup subgroup supergroup

-- | Remove a group (subgroup) from another group (supergroup). If subgroup is not a direct
-- member of supergroup, do nothing. Only a GroupAdmin may do this.
removeGroupFromGroup :: MonadQuery e v m => ServiceGroup -> ServiceGroup -> m ()
removeGroupFromGroup subgroup supergroup = do
    assertAuth $ hasGroup GroupGroupAdmin
    queryA $ T.removeGroupFromGroup subgroup supergroup

-- | List all groups a persona belongs to, directly or indirectly. If p is a member of g1,
-- g1 is a member of g2, and g2 is a member of g3, [g1, g2, g3] will be returned.
-- Only the user owning the persona or a GroupAdmin may do this.
personaGroups :: MonadQuery e v m => Persona -> m [ServiceGroup]
personaGroups persona = do
    assertAuth $ hasUserId (persona ^. personaUid) <||> hasGroup GroupGroupAdmin
    queryA $ T.personaGroups (persona ^. personaId)


-- * agents and groups

assignGroup :: MonadQuery e v m => Agent -> Group -> m ()
assignGroup agent group = do
    guardWriteMsg "assignGroup" (GroupAdmin %% GroupAdmin)
    queryA $ T.assignGroup agent group

unassignGroup :: MonadQuery e v m => Agent -> Group -> m ()
unassignGroup agent group = do
    guardWriteMsg "unassignGroup" (GroupAdmin %% GroupAdmin)
    queryA $ T.unassignGroup agent group

agentGroups :: MonadQuery e v m => Agent -> m [Group]
agentGroups agent = do
    taintMsg "agentGroups" (GroupAdmin \/ agent %% GroupAdmin /\ agent)
    queryA (T.agentGroups agent)


-- * Sybil attack prevention

-- | Generate a captcha. Returns a pair of 'CaptchaId' and the binary image data in PNG format.
-- The correct solution to the captcha is stored in the DB. Does not require any privileges.
makeCaptcha :: MonadAction e v m => m (CaptchaId, ImageData)
makeCaptcha = do
    cid    <- freshCaptchaId
    random <- freshRandom20
    (imgdata, discardWhitespace -> solution) <- U.unsafeLiftIO $ Sybil.generateCaptcha random
    queryA $ T.storeCaptcha cid solution
    loggerA DEBUG $ concat ["Generated visual captcha: ", show cid, ", solution = ", cs solution]
    pure (cid, imgdata)

-- | Argument must be an espeak voice installed on the server system.  Try "en", "de", "fi", "ru" or
-- call `espeak --voices` for a complete list.
makeAudioCaptcha :: MonadAction e v m => String -> m (CaptchaId, SBS)
makeAudioCaptcha eSpeakVoice = do
    cid    <- freshCaptchaId
    random <- freshRandom20
    (wav, discardWhitespace -> solution) <- Sybil.generateAudioCaptcha eSpeakVoice random
    queryA $ T.storeCaptcha cid solution
    loggerA DEBUG $ concat ["Generated audio captcha: ", show cid, ", solution = ", cs solution]
    pure (cid, wav)

-- | Submit a solution to a captcha, returning whether or not the solution is correct.
-- If the solution is wrong, delete captcha to prevent multiple guesses.
-- Throws 'NoSuchCaptchaId' if the given 'CaptchaId' doesn't exist in the DB (either because it
-- never did or because it was deleted). Does not require any privileges.
solveCaptcha :: MonadQuery e v m => CaptchaId -> ST -> m Bool
solveCaptcha cid (discardWhitespace -> solution) = do
    solutionCorrect <- queryA $ T.solveCaptcha cid solution
    loggerA DEBUG $ concat ["Captcha solution submitted: ", show cid, ", submitted solution = ",
                            cs solution, ", correct = ", show solutionCorrect]
    unless solutionCorrect $ deleteCaptcha cid
    return solutionCorrect

-- | Delete a captcha and its solution from the DB. Throws 'NoSuchCaptchaId' if the given
-- 'CaptchaId' doesn't exist in the DB (either because it never did or because it was deleted due
-- to garbage collection or a prior call to this action). Does not require any privileges.
deleteCaptcha :: MonadQuery e v m => CaptchaId -> m ()
deleteCaptcha = queryA . T.deleteCaptcha

discardWhitespace :: ST -> ST
discardWhitespace = ST.concat . ST.words


-- * garbage collection

collectGarbage :: (Exception (ActionError e), MonadQuery e v m) => m ()
collectGarbage = do
    loggerA DEBUG "starting garbage collection."
    guardWriteMsg "collectGarbage" (GroupAdmin %% GroupAdmin)

    queryA T.garbageCollectThentosSessions
    queryA T.garbageCollectServiceSessions

    config <- getConfig
    let userExpiry     = config >>. (Proxy :: Proxy '["user_reg_expiration"])
        passwordExpiry = config >>. (Proxy :: Proxy '["pw_reset_expiration"])
        emailExpiry    = config >>. (Proxy :: Proxy '["email_change_expiration"])
        captchaExpiry  = config >>. (Proxy :: Proxy '["captcha_expiration"])
    queryA $ T.garbageCollectUnconfirmedUsers userExpiry
    queryA $ T.garbageCollectEmailChangeTokens emailExpiry
    queryA $ T.garbageCollectPasswordResetTokens passwordExpiry
    queryA $ T.garbageCollectCaptchas captchaExpiry

    loggerA DEBUG "garbage collection complete!"
