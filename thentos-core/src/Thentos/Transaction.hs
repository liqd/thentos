{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Thentos.Transaction
where

import Control.Applicative ((<$>))
import Control.Exception.Lifted (throwIO)
import Control.Lens ((^.))
import Control.Monad (void, liftM)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Database.PostgreSQL.Simple         (Only(..), execute)
import Database.PostgreSQL.Simple.Errors  (ConstraintViolation(UniqueViolation))
import Database.PostgreSQL.Simple.SqlQQ   (sql)
import Data.Typeable (Typeable)

import Thentos.Types
import Thentos.Transaction.Core

import qualified Data.Set as Set


-- * user

lookupUser :: UserId -> ThentosQuery e (UserId, User)
lookupUser uid = do
    users <- queryT [sql| SELECT name, password, email
                          FROM users
                          WHERE id = ? |] (Only uid)
    case users of
      [(name, pwd, email)] -> return (uid, User name pwd email)
      []                   -> throwError NoSuchUser
      _                    -> impossible "lookupUser: multiple results"

lookupUserByName :: UserName -> ThentosQuery e (UserId, User)
lookupUserByName uname = do
    users <- queryT [sql| SELECT id, name, password, email
                          FROM users
                          WHERE name = ? |] (Only uname)
    case users of
      [(uid, name, pwd, email)] -> return (uid, User name pwd email)
      []                        -> throwError NoSuchUser
      _                         -> impossible "lookupUserByName: multiple users"


lookupUserByEmail :: UserEmail -> ThentosQuery e (UserId, User)
lookupUserByEmail email = do
    users <- queryT [sql| SELECT id, name, password
                          FROM users
                          WHERE email = ? |] (Only email)
    case users of
      [(uid, name, pwd)] -> return (uid, User name pwd email)
      []                 -> throwError NoSuchUser
      _                  -> impossible "lookupUserByEmail: multiple users"

-- | Actually add a new user. The user may already have an ID, otherwise the DB will automatically
-- create one (auto-increment). NOTE that mixing calls with 'Just' an ID with those without
-- is a very bad idea and will quickly lead to errors!
addUserPrim :: Maybe UserId -> User -> Bool -> ThentosQuery e UserId
addUserPrim mUid user confirmed = do
    res <- queryT [sql| INSERT INTO users (id, name, password, email, confirmed)
                        VALUES (?, ?, ?, ?, ?)
                        RETURNING id |]
            (orDefault mUid, user ^. userName, user ^. userPassword, user ^. userEmail, confirmed)
    case res of
        [Only uid] -> return uid
        []         -> impossible "addUserPrim created user without ID"
        _          -> impossible "addUserPrim created multiple users"

-- | Add a new user and return the new user's 'UserId'.
-- Ensures that user name and email address are unique.
addUser :: (Show e, Typeable e) => User -> ThentosQuery e UserId
addUser user = addUserPrim Nothing user True

addUnconfirmedUserPrim :: ConfirmationToken -> User -> Maybe UserId -> ThentosQuery e UserId
addUnconfirmedUserPrim token user mUid = do
    uid <- addUserPrim mUid user False
    void $ execT [sql| INSERT INTO user_confirmation_tokens (id, token)
                       VALUES (?, ?) |] (uid, token)
    return uid

-- | Add a new unconfirmed user (i.e. one whose email address we haven't confirmed yet).
-- Ensures that user name and email address are unique.
addUnconfirmedUser :: (Show e, Typeable e) => ConfirmationToken -> User -> ThentosQuery e UserId
addUnconfirmedUser token user = addUnconfirmedUserPrim token user Nothing

-- | Add a new unconfirmed user, assigning a specific ID to the new user.
-- Ensures that ID, user name and email address are unique.
--
-- BE CAREFUL regarding the source of the specified user ID. If it comes from a backend process
-- (such as the A3 backend), it should be safe. But if a user/external API can provide it, that
-- would leak information about the (non-)existence of IDs in our db.
addUnconfirmedUserWithId :: ConfirmationToken -> User -> UserId -> ThentosQuery e ()
addUnconfirmedUserWithId token user uid = void $ addUnconfirmedUserPrim token user $ Just uid

finishUserRegistration :: Timeout -> ConfirmationToken -> ThentosQuery e UserId
finishUserRegistration timeout token = do
    res <- queryT [sql|
        UPDATE users SET confirmed = true
        FROM user_confirmation_tokens
        WHERE users.id = user_confirmation_tokens.id
            AND timestamp + ? > now()
            AND token = ?;

        DELETE FROM user_confirmation_tokens
        WHERE token = ? AND timestamp + ? > now()
        RETURNING id;
    |] (timeout, token, token, timeout)
    case res of
        [] -> throwError NoSuchToken
        [Only uid] -> return uid
        _ -> impossible "repeated user confirmation token"

-- | Confirm a user based on the 'UserId' rather than the 'ConfirmationToken.'
finishUserRegistrationById :: UserId -> ThentosQuery e ()
finishUserRegistrationById uid = do
    c <- execT [sql|
    UPDATE users SET confirmed = true WHERE id = ?;
    DELETE FROM user_confirmation_tokens WHERE id = ?;
    |] (uid, uid)
    case c of
        1 -> return ()
        0 -> throwError NoSuchPendingUserConfirmation
        _ -> impossible "finishUserRegistrationById: id uniqueness violation"

-- | Add a password reset token.  Return the user whose password this token can change.
addPasswordResetToken :: UserEmail -> PasswordResetToken -> ThentosQuery e User
addPasswordResetToken email token = do
    (uid, user) <- lookupUserByEmail email
    void $ execT [sql| INSERT INTO password_reset_tokens (token, uid)
                VALUES (?, ?) |] (token, uid)
    return user

-- | Change a password with a given password reset token and remove the token.  Throw an error if
-- the token does not exist or has expired.
resetPassword :: Timeout -> PasswordResetToken -> HashedSecret UserPass -> ThentosQuery e ()
resetPassword timeout token newPassword = do
    modified <- execT [sql| UPDATE users
                            SET password = ?
                            FROM password_reset_tokens
                            WHERE password_reset_tokens.timestamp + ? > now()
                            AND users.id = password_reset_tokens.uid
                            AND password_reset_tokens.token = ?
                      |] (newPassword, timeout, token)
    case modified of
        1 -> return ()
        0 -> throwError NoSuchToken
        _ -> impossible "password reset token exists multiple times"

addUserEmailChangeRequest :: UserId -> UserEmail -> ConfirmationToken -> ThentosQuery e ()
addUserEmailChangeRequest uid newEmail token = do
    void $ execT [sql| INSERT INTO email_change_tokens (token, uid, new_email)
                VALUES (?, ?, ?) |] (token, uid, newEmail)

-- | Change email with a given token and remove the token.  Throw an error if the token does not
-- exist or has expired.
confirmUserEmailChange :: Timeout -> ConfirmationToken -> ThentosQuery e ()
confirmUserEmailChange timeout token = do
    modified <- execT [sql| UPDATE users
                            SET email = email_change_tokens.new_email
                            FROM email_change_tokens
                            WHERE timestamp + ? > now()
                            AND users.id = email_change_tokens.uid
                            AND email_change_tokens.token = ?
                      |] (timeout, token)
    case modified of
        1 -> return ()
        0 -> throwError NoSuchToken
        _ -> impossible "email change token exists multiple times"

-- | Look up an email change token. Does not verify that the token is still
-- valid, just retrieves it from the database.
lookupEmailChangeToken :: ConfirmationToken -> ThentosQuery e ((UserId, UserEmail), Timestamp)
lookupEmailChangeToken token = do
    rs <- queryT [sql| SELECT uid, new_email, timestamp
                       FROM email_change_tokens
                       WHERE token = ? |] (Only token)
    case rs of
        [(uid, email, ts)] -> return ((uid, email), ts)
        []                 -> throwError NoSuchToken
        _                  -> impossible "repeated email change token"


data UpdateUserFieldOp =
    UpdateUserFieldName UserName
  | UpdateUserFieldEmail UserEmail
  | UpdateUserFieldPassword (HashedSecret UserPass)
  deriving (Eq, Show)

-- | Update one of the attributes stored for a user.
updateUserField :: UserId -> UpdateUserFieldOp -> ThentosQuery e ()
updateUserField uid op = do
    modified <- q
    case modified of
        1 -> return ()
        0 -> throwError NoSuchUser
        _ -> impossible "updateUserField: unique constraint on id violated"
  where
    q = case op of
        UpdateUserFieldName n ->
            execT [sql| UPDATE users SET name = ? WHERE id = ? |] (n, uid)
        UpdateUserFieldEmail e ->
            execT [sql| UPDATE users SET email = ? WHERE id = ? |] (e, uid)
        UpdateUserFieldPassword p ->
            execT [sql| UPDATE users SET password = ? WHERE id = ? |] (p, uid)


-- | Update attributes stored for a user.
updateUserFields :: UserId -> [UpdateUserFieldOp] -> ThentosQuery e ()
updateUserFields uid ups = do
    conn <- ask
    e <- catchViolation catcher . liftIO . liftM Right $ mapM (exec conn) ups
    case e of
        Left err -> throwError err
        Right ns | all (== 1) ns -> return ()
                 | all (== 0) ns -> throwError NoSuchUser
                 | otherwise -> impossible $ "updateUserFields: updated " ++ show ns
  where
    exec conn op = case op of
        UpdateUserFieldName n ->
            execute conn [sql| UPDATE users SET name = ? WHERE id = ? |] (n, uid)
        UpdateUserFieldEmail e ->
            execute conn [sql| UPDATE users SET email = ? WHERE id = ? |] (e, uid)
        UpdateUserFieldPassword p ->
            execute conn [sql| UPDATE users SET password = ? WHERE id = ? |] (p, uid)

-- | Delete user with given 'UserId'.  Throw an error if user does not exist.
deleteUser :: UserId -> ThentosQuery e ()
deleteUser uid
    = execT [sql| DELETE FROM users WHERE id = ? |] (Only uid) >>= \ x -> case x of
      1 -> return ()
      0 -> throwError NoSuchUser
      _ -> impossible "deleteUser: unique constraint on id violated"


-- * service

allServiceIds :: ThentosQuery e [ServiceId]
allServiceIds = map fromOnly <$> queryT [sql| SELECT id FROM services |] ()

lookupService :: ServiceId -> ThentosQuery e (ServiceId, Service)
lookupService sid = do
    services <- queryT [sql| SELECT key, owner_user, owner_service, name, description
                             FROM services
                             WHERE id = ? |] (Only sid)
    service <- case services of
        [(key, ownerU, ownerS, name, desc)] ->
            let owner = makeAgent ownerU ownerS
            in return $ Service key owner Nothing name desc
        []                         -> throwError NoSuchService
        _                          -> impossible "lookupService: multiple results"
    return (sid, service)

-- | Add new service.
addService ::
    Agent -> ServiceId -> HashedSecret ServiceKey -> ServiceName
    -> ServiceDescription -> ThentosQuery e ()
addService (UserA uid) sid secret name description = void $
    execT [sql| INSERT INTO services (id, owner_user, name, description, key)
                VALUES (?, ?, ?, ?, ?)
          |] (sid, uid, name, description, secret)
addService (ServiceA ownerSid) sid secret name description = void $
    execT [sql| INSERT INTO services (id, owner_service, name, description, key)
                VALUES (?, ?, ?, ?, ?)
          |] (sid, ownerSid, name, description, secret)

-- | Delete service with given 'ServiceId'.  Throw an error if service does not exist.
deleteService :: ServiceId -> ThentosQuery e ()
deleteService sid = do
    deletedCount <- execT [sql| DELETE FROM services
                                WHERE id = ? |] (Only sid)
    case deletedCount of
        0 -> throwError NoSuchService
        1 -> return ()
        _ -> impossible "deleteService: multiple results"

-- Register a user to grant them access to a service. Throws an error if the user is already
-- registered for the service.
registerUserWithService :: UserId -> ServiceId -> ServiceAccount -> ThentosQuery e ()
registerUserWithService uid sid (ServiceAccount anonymous) = void $
    execT [sql| INSERT INTO user_services (uid, sid, anonymous)
                VALUES (?, ?, ?) |] (uid, sid, anonymous)

-- Unregister a user from accessing a service. No-op if the user was not registered for the
-- service.
unregisterUserFromService :: UserId -> ServiceId -> ThentosQuery e ()
unregisterUserFromService uid sid = void $
    execT [sql| DELETE FROM user_services WHERE uid = ? AND sid = ? |] (uid, sid)


-- * thentos and service session

-- | Lookup session.  If session does not exist or has expired, throw an error.  If it does exist,
-- bump the expiry time and return session with bumped expiry time.
lookupThentosSession ::
     ThentosSessionToken -> ThentosQuery e (ThentosSessionToken, ThentosSession)
lookupThentosSession token = do
    void $ execT [sql| UPDATE thentos_sessions
                       SET end_ = now()::timestamptz + period
                       WHERE token = ? AND end_ >= now()
                 |] (Only token)
    sesss <- queryT [sql| SELECT uid, sid, start, end_, period FROM thentos_sessions
                          WHERE token = ? AND end_ >= now()
                    |] (Only token)
    case sesss of
        [(uid, sid, start, end, period)] ->
            let agent = makeAgent uid sid
            in return ( token
                      , ThentosSession agent start end period Set.empty
                      )
        []                          -> throwError NoSuchThentosSession
        _                           -> impossible "lookupThentosSession: multiple results"

-- | Start a new thentos session. Start time is set to now, end time is calculated based on the
-- specified 'Timeout'. If the agent is a user, this new session is added to their existing
-- sessions.
-- FIXME not implemented: If the agent is a service with an existing session, its session is
-- replaced.
startThentosSession :: ThentosSessionToken -> Agent -> Timeout -> ThentosQuery e ()
startThentosSession tok (UserA uid) period =
    void $ execT [sql| INSERT INTO thentos_sessions (token, uid, start, end_, period)
                       VALUES (?, ?, now(), now() + ?, ?)
                 |] (tok, uid, period, period)
startThentosSession tok (ServiceA sid) period = void $ execT
    [sql| INSERT INTO thentos_sessions (token, sid, start, end_, period)
          VALUES (?, ?, now(), now() + ?, ?)|]
            (tok, sid, period, period)

-- | End thentos session and all associated service sessions.
-- If thentos session does not exist or has expired, remove it just the same.
--
-- Always call this transaction if you want to clean up a session (e.g., from a garbage collection
-- transaction).  This way in the future, you can replace this transaction easily by one that does
-- not actually destroy the session, but move it to an archive.
endThentosSession :: ThentosSessionToken -> ThentosQuery e ()
endThentosSession tok =
    void $ execT [sql| DELETE FROM thentos_sessions WHERE token = ?
                 |] (Only tok)

-- | Like 'lookupThentosSession', but for 'ServiceSession'.  Bump both service and associated
-- thentos session.  If the service session is still active, but the associated thentos session has
-- expired, update service sessions expiry time to @now@ and throw 'NoSuchThentosSession'.
lookupServiceSession :: ServiceSessionToken -> ThentosQuery e (ServiceSessionToken, ServiceSession)
lookupServiceSession token = do
    sessions <- queryT
        [sql| SELECT service, start, end_, period, thentos_session_token, meta
              FROM service_sessions
              WHERE token = ? |] (Only token)
    case sessions of
        []        -> throwError NoSuchServiceSession
        [(service, start, end, period, thentosSessionToken, meta)] ->
            return (token, ServiceSession service start end period thentosSessionToken meta)
        _         -> impossible "multiple sessions with the same token"

-- | Like 'startThentosSession' for service sessions.  Bump associated thentos session.  Throw an
-- error if thentos session lookup fails.  If a service session already exists for the given
-- 'ServiceId', return its token.
startServiceSession ::
    ThentosSessionToken -> ServiceSessionToken -> ServiceId
    -> Timeout -> ThentosQuery e ()
startServiceSession thentosSessionToken token sid timeout =
    void $ execT [sql| INSERT INTO service_sessions
                        (token, thentos_session_token, start, end_, period, service, meta)
                       VALUES (?, ?, now(), now() + ?, ?, ?,
                            (SELECT users.name FROM users, thentos_sessions WHERE
                             users.id = thentos_sessions.uid
                                AND thentos_sessions.token = ?)
                            ) |]
                (token, thentosSessionToken, timeout, timeout, sid, thentosSessionToken)

-- | Like 'endThentosSession' for service sessions (see there).  If thentos session or service
-- session do not exist or have expired, remove the service session just the same, but never thentos
-- session.
endServiceSession :: ServiceSessionToken -> ThentosQuery e ()
endServiceSession token = do
    deleted <- execT
        [sql| DELETE FROM service_sessions WHERE token = ? |] (Only token)
    case deleted of
        0 -> throwError NoSuchServiceSession
        1 -> return ()
        _ -> impossible "multiple service sessions with same token"


-- * agents, roles, and groups

-- | Add a new role to the roles defined for an 'Agent'.  If 'Role' is already assigned to
-- 'Agent', do nothing.
assignRole :: Agent -> Role -> ThentosQuery e ()
assignRole agent role = case agent of
    ServiceA sid -> catchViolation catcher' $
        void $ execT [sql| INSERT INTO service_roles (sid, role)
                           VALUES (?, ?) |] (sid, role)
    UserA uid  -> do
        catchViolation catcher' $ void $
            execT [sql| INSERT INTO user_roles (uid, role)
                        VALUES (?, ?) |] (uid, role)
  where
    catcher' _ (UniqueViolation "user_roles_uid_role_key") = return ()
    catcher' _ (UniqueViolation "service_roles_sid_role_key") = return ()
    catcher' e _                                           = throwIO e

-- | Remove a 'Role' from the roles defined for an 'Agent'.  If 'Role' is not assigned to 'Agent',
-- do nothing.
unassignRole :: Agent -> Role -> ThentosQuery e ()
unassignRole agent role = case agent of
    ServiceA sid -> void $ execT
        [sql| DELETE FROM service_roles WHERE sid = ? AND role = ? |] (sid, role)
    UserA uid  -> do
        void $ execT [sql| DELETE FROM user_roles WHERE uid = ? AND role = ? |]
                           (uid, role)

-- | All 'Role's of an 'Agent'.  If 'Agent' does not exist or has no roles, return an empty list.
agentRoles :: Agent -> ThentosQuery e (Set.Set Role)
agentRoles agent = case agent of
    ServiceA sid -> do
        roles <- queryT [sql| SELECT role FROM service_roles WHERE sid = ? |]
                        (Only sid)
        return . Set.fromList $ map fromOnly roles
    UserA uid  -> do
        roles <- queryT [sql| SELECT role FROM user_roles WHERE uid = ? |] (Only uid)
        return . Set.fromList . map fromOnly $ roles

-- * garbage collection

-- | Go through "thentos_sessions" table and find all expired sessions.
garbageCollectThentosSessions :: ThentosQuery e ()
garbageCollectThentosSessions = void $ execT [sql|
    DELETE FROM thentos_sessions WHERE end_ < now()
    |] ()

garbageCollectServiceSessions :: ThentosQuery e ()
garbageCollectServiceSessions = void $ execT [sql|
    DELETE FROM service_sessions WHERE end_ < now()
    |] ()

-- | Remove all expired unconfirmed users from db.
garbageCollectUnconfirmedUsers :: Timeout -> ThentosQuery e ()
garbageCollectUnconfirmedUsers timeout = void $ execT [sql|
    DELETE FROM "users" WHERE created < now() - interval '? seconds' AND confirmed = false;
    |] (Only (round timeout :: Integer))

-- | Remove all expired password reset requests from db.
garbageCollectPasswordResetTokens :: Timeout -> ThentosQuery e ()
garbageCollectPasswordResetTokens timeout = void $ execT [sql|
    DELETE FROM "password_reset_tokens" WHERE timestamp < now() - interval '? seconds';
    |] (Only (round timeout :: Integer))

-- | Remove all expired email change requests from db.
garbageCollectEmailChangeTokens :: Timeout -> ThentosQuery e ()
garbageCollectEmailChangeTokens timeout = void $ execT [sql|
    DELETE FROM "email_change_tokens" WHERE timestamp < now() - interval '? seconds';
    |] (Only (round timeout :: Integer))


-- * helpers

-- | Throw an error from a situation which (we believe) will never arise.
impossible :: String -> a
impossible msg = error $ "Impossible error: " ++ msg
