{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Thentos.Transaction.Transactions
where

import Control.Exception.Lifted (throwIO)
import Data.Monoid (mempty)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Control.Applicative ((<$>))
import Control.Lens ((^.))
import Control.Monad (void)
import Control.Monad.Catch (catch)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple        (Only(..))
import Database.PostgreSQL.Simple.SqlQQ  (sql)
import Database.PostgreSQL.Simple.Errors (ConstraintViolation(UniqueViolation))
import System.Random (randomIO)

import Thentos.Types
import Thentos.Transaction.Core


-- * user

freshUserId :: ThentosQuery e UserId
freshUserId = error "src/Thentos/Transaction/Transactions.hs:13"

lookupUser :: UserId -> ThentosQuery e (UserId, User)
lookupUser uid = do
    users <- queryT [sql| SELECT name, password, email
                          FROM users
                          WHERE id = ? |] (Only uid)
    user <- case users of
      [user] -> return user
      []     -> throwError NoSuchUser
      _      -> impossible "lookupUser: multiple results"
    return (uid, user)

lookupUserByName :: UserName -> ThentosQuery e (UserId, User)
lookupUserByName name = do
    users <- queryT [sql| SELECT id, name, password, email
                          FROM users
                          WHERE name = ? |] (Only name)
    case users of
      [(uid, uname, pwd, email)] -> return (uid, User uname pwd email mempty mempty)
      []                        -> throwError NoSuchUser
      _                         -> impossible "lookupUserByName: multiple users"


lookupUserByEmail :: UserEmail -> ThentosQuery e (UserId, User)
lookupUserByEmail email = do
    users <- queryT [sql| SELECT id, name, password
                          FROM users
                          WHERE email = ? |] (Only email)
    case users of
      [(uid, name, pwd)] -> return (uid, User name pwd email mempty mempty)
      []                 -> throwError NoSuchUser
      _                  -> impossible "lookupUserByEmail: multiple users"

-- | Actually add a new user who already has an ID.
addUserPrim :: UserId -> User -> ThentosQuery e ()
addUserPrim uid user = do
    void $ execT [sql| INSERT INTO users (id, name, password, email, confirmed)
                       VALUES (?, ?, ?, ?, true) |] ( uid
                                                    , user ^. userName
                                                    , user ^. userPassword
                                                    , user ^. userEmail
                                                    )

-- | Add a new user and return the new user's 'UserId'.
-- Ensures that user name and email address are unique.
addUser :: (Show e, Typeable e) => User -> ThentosQuery e UserId
addUser user = go (5 :: Int)
  where
    go 0 = error "addUser: could not generate unique id"
    go n = liftIO randomIO >>= \uid -> (addUserPrim uid user >> return uid) `catch` f
      where f UserIdAlreadyExists = go (n - 1)
            f e                   = throwError e

-- | Add a new unconfirmed user (i.e. one whose email address we haven't confirmed yet).
-- Ensures that user name and email address are unique.
addUnconfirmedUser :: (Show e, Typeable e) => ConfirmationToken -> User -> ThentosQuery e UserId
addUnconfirmedUser token user = go (5 :: Int)
  where
    go 0 = error "addUnconfirmedUser: could not generate unique id"
    go n = liftIO randomIO >>= \uid -> (addUnconfirmedUserWithId token user uid
                                        >> return uid) `catch` f
      where f UserIdAlreadyExists = go (n - 1)
            f e                   = throwError e

-- | Add a new unconfirmed user, assigning a specific ID to the new user.
-- Ensures that ID, user name and email address are unique.
--
-- BE CAREFUL regarding the source of the specified user ID. If it comes from a backend process
-- (such as the A3 backend), it should be safe. But if a user/external API can provide it, that
-- would leak information about the (non-)existence of IDs in our db.
addUnconfirmedUserWithId :: ConfirmationToken -> User -> UserId -> ThentosQuery e ()
addUnconfirmedUserWithId token user uid = void $ execT [sql|
    INSERT INTO users (id, name, password, email, confirmed)
    VALUES (?, ?, ?, ?, false);
    INSERT INTO user_confirmation_tokens (id, token)
    VALUES (?, ?);
    |] ( uid, user ^. userName, user ^. userPassword, user ^. userEmail
       , uid, token )

finishUserRegistration :: Timestamp -> Timeout -> ConfirmationToken -> ThentosQuery e UserId
finishUserRegistration = error "src/Thentos/Transaction/Transactions.hs:67"

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
  | UpdateUserFieldInsertService ServiceId ServiceAccount
  | UpdateUserFieldDropService ServiceId
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
        _ -> error $ "updateUserField op not implemented: " ++ show op

-- | Update attributes stored for a user.
-- FIXME: should be transactional
updateUserFields :: UserId -> [UpdateUserFieldOp] -> ThentosQuery e ()
updateUserFields uid = mapM_ (updateUserField uid)

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
    services <- queryT [sql| SELECT key, owner, name, description
                             FROM services
                             WHERE id = ? |] (Only sid)
    service <- case services of
        [(key, owner, name, desc)] -> return $ Service key owner Nothing name desc Map.empty
        []                         -> throwError NoSuchService
        _                          -> impossible "lookupService: multiple results"
    return (sid, service)

-- | Add new service.
-- FIXME: the agent has to be a user for now
addService ::
    Agent -> ServiceId -> HashedSecret ServiceKey -> ServiceName
    -> ServiceDescription -> ThentosQuery e ()
addService agent sid secret name description = void $
    execT [sql| INSERT INTO services (id, owner, name, description, key)
                VALUES (?, ?, ?, ?, ?)
          |] (sid, agent, name, description, secret)

-- | Delete service with given 'ServiceId'.  Throw an error if service does not exist.
deleteService :: ServiceId -> ThentosQuery e ()
deleteService sid = do
    deletedCount <- execT [sql| DELETE FROM services
                                WHERE id = ? |] (Only sid)
    case deletedCount of
        0 -> throwError NoSuchService
        1 -> return ()
        _ -> impossible "deleteService: multiple results"


-- * thentos and service session

-- | Lookup session.  If session does not exist or has expired, throw an error.  If it does exist,
-- bump the expiry time and return session with bumped expiry time.
lookupThentosSession ::
     ThentosSessionToken -> ThentosQuery e (ThentosSessionToken, ThentosSession)
lookupThentosSession token = do
    void $ execT [sql| UPDATE user_sessions
                       SET end_ = now()::timestamptz + period
                       WHERE token = ? AND end_ >= now()
                 |] (Only token)
    sesss <- queryT [sql| SELECT uid, start, end_, period FROM user_sessions
                          WHERE token = ? AND end_ >= now()
                    |] (Only token)
    case sesss of
        [(uid, start, end, period)] ->
             return ( token
                    , ThentosSession (UserA uid) start end period Set.empty
                    )
        []                          -> throwError NoSuchThentosSession
        _                           -> impossible "lookupThentosSession: multiple results"

-- | Start a new thentos session.  Start and end time have to be passed explicitly.
-- If the agent is a user, this new session is added to their existing sessions.
-- If the agent is a service with an existing session, its session is replaced.
startThentosSession :: ThentosSessionToken -> Agent -> Timeout -> ThentosQuery e ()
startThentosSession tok agent period =
    void $ execT [sql| INSERT INTO user_sessions (token, uid, start, end_, period)
                       VALUES (?, ?, now(), now() + ?, ?)
                 |] (tok, agent, period, period)

-- | End thentos session and all associated service sessions.
-- If thentos session does not exist or has expired, remove it just the same.
--
-- Always call this transaction if you want to clean up a session (e.g., from a garbage collection
-- transaction).  This way in the future, you can replace this transaction easily by one that does
-- not actually destroy the session, but move it to an archive.
endThentosSession :: ThentosSessionToken -> ThentosQuery e ()
endThentosSession tok =
    void $ execT [sql| DELETE FROM user_sessions WHERE token = ?
                 |] (Only tok)

-- | Like 'lookupThentosSession', but for 'ServiceSession'.  Bump both service and associated
-- thentos session.  If the service session is still active, but the associated thentos session has
-- expired, update service sessions expiry time to @now@ and throw 'NoSuchThentosSession'.
lookupServiceSession :: ServiceSessionToken -> ThentosQuery e (ServiceSessionToken, ServiceSession)
lookupServiceSession token = do
    sessions <- queryT
        [sql| SELECT * FROM WHERE token = ? |] (Only token)
    case sessions of
        []        -> throwError NoSuchServiceSession
        [session] -> return (token, session)
        _         -> impossible "multiple sessions with the same token"

-- | Like 'startThentosSession' for service sessions.  Bump associated thentos session.  Throw an
-- error if thentos session lookup fails.  If a service session already exists for the given
-- 'ServiceId', return its token.
startServiceSession ::
    ThentosSessionToken -> ServiceSessionToken -> ServiceId
    -> Timeout -> ThentosQuery e ()
startServiceSession thentosSessionToken token sid timeout =
    void $ execT [sql| INSERT INTO service_sessions
                        (token, thentos_session_token, start, end_, period, service)
                       VALUES (?, ?, now(), now() + ?, ?, ?) |]
                (token, thentosSessionToken, timeout, timeout, sid)

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

-- | For a given service and user id, look up all groups the user has in the context of that service
-- from the service's group tree, and collect them into a list.
flattenGroups :: Service -> UserId -> [Group]
flattenGroups = error "src/Thentos/Transaction/Transactions.hs:10"

-- | Add a new role to the roles defined for an 'Agent'.  If 'Role' is already assigned to
-- 'Agent', do nothing.
assignRole :: Agent -> Role -> ThentosQuery e ()
assignRole agent role = case agent of
    ServiceA _ -> error "assignRole not implemented for services"
    UserA uid  -> do
        catchViolation catcher $ void $
            execT [sql| INSERT INTO user_roles (uid, role)
                        VALUES (?, ?) |] (uid, role)
  where
    catcher _ (UniqueViolation "user_roles_uid_role_key") = return ()
    catcher e _                                           = throwIO e

-- | Remove a 'Role' from the roles defined for an 'Agent'.  If 'Role' is not assigned to 'Agent',
-- do nothing.
unassignRole :: Agent -> Role -> ThentosQuery e ()
unassignRole agent role = case agent of
    ServiceA _ -> error "unassignRole not implemented for services"
    UserA uid  -> do
        void $ execT [sql| DELETE FROM user_roles WHERE uid = ? AND role = ? |]
                           (uid, role)

-- | All 'Role's of an 'Agent'.  If 'Agent' does not exist or has no roles, return an empty list.
agentRoles :: Agent -> ThentosQuery e (Set.Set Role)
agentRoles agent = case agent of
    ServiceA _ -> error "agentRoles not implemented for services"
    UserA uid  -> do
        roles <- queryT [sql| SELECT role FROM user_roles WHERE uid = ? |] (Only uid)
        return $ Set.fromList roles

-- * garbage collection

-- | Go through "user_sessions" table and find all expired sessions.
garbageCollectThentosSessions :: ThentosQuery e ()
garbageCollectThentosSessions = error "src/Thentos/Transaction/Transactions.hs:161"

garbageCollectServiceSessions :: ThentosQuery e ()
garbageCollectServiceSessions = error "src/Thentos/Transaction/Transactions.hs:169"

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
