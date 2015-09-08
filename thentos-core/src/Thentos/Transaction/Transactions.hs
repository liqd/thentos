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

flattenGroups :: Service -> UserId -> [Group]
flattenGroups = error "src/Thentos/Transaction/Transactions.hs:10"

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

addUserPrim :: UserId -> User -> ThentosQuery e ()
addUserPrim uid user = do
    void $ execT [sql| INSERT INTO users (id, name, password, email, confirmed)
                       VALUES (?, ?, ?, ?, true) |] ( uid
                                                    , user ^. userName
                                                    , user ^. userPassword
                                                    , user ^. userEmail
                                                    )

-- | Add a user with a random ID.
addUser :: (Show e, Typeable e) => User -> ThentosQuery e UserId
addUser user = go (5 :: Int)
  where
    go 0 = error "addUser: could not generate unique id"
    go n = liftIO randomIO >>= \uid -> (addUserPrim uid user >> return uid) `catch` f
      where f UserIdAlreadyExists = go (n - 1)
            f e                   = throwError e


addUnconfirmedUser :: (Show e, Typeable e) => ConfirmationToken -> User -> ThentosQuery e UserId
addUnconfirmedUser token user = go (5 :: Int)
  where
    go 0 = error "addUnconfirmedUser: could not generate unique id"
    go n = liftIO randomIO >>= \uid -> (addUnconfirmedUserWithId token user uid
                                        >> return uid) `catch` f
      where f UserIdAlreadyExists = go (n - 1)
            f e                   = throwError e


addUnconfirmedUserWithId :: ConfirmationToken -> User -> UserId -> ThentosQuery e ()
addUnconfirmedUserWithId token user uid = void $ execT [sql|
    INSERT INTO users (id, name, password, email, confirmed)
    VALUES (?, ?, ?, ?, false);
    INSERT INTO user_confirmation_tokens (id, token)
    VALUES (?, ?);
    |] ( uid, user ^. userName, user ^. userPassword, user ^. userEmail
       , uid, token )

finishUserRegistration ::
    Timestamp -> Timeout -> ConfirmationToken -> ThentosQuery e UserId
finishUserRegistration = error "src/Thentos/Transaction/Transactions.hs:67"

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

addPasswordResetToken :: UserEmail -> PasswordResetToken -> ThentosQuery e User
addPasswordResetToken email token = do
    (uid, user) <- lookupUserByEmail email
    void $ execT [sql| INSERT INTO password_reset_tokens (token, uid)
                VALUES (?, ?) |] (token, uid)
    return user

resetPassword ::
    Timeout -> PasswordResetToken -> HashedSecret UserPass -> ThentosQuery e ()
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

lookupEmailChangeToken ::
    ConfirmationToken -> ThentosQuery e ((UserId, UserEmail), Timestamp)
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

-- FIXME: should be transactional
updateUserFields :: UserId -> [UpdateUserFieldOp] -> ThentosQuery e ()
updateUserFields uid = mapM_ (updateUserField uid)

deleteUser :: UserId -> ThentosQuery e ()
deleteUser uid
    = execT [sql| DELETE FROM users WHERE id = ? |] (Only uid) >>= \ x -> case x of
      1 -> return ()
      0 -> throwError NoSuchUser
      _ -> impossible "deleteUser: unique constraint on id violated"

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

-- FIXME: the agent has to be a user for now
addService ::
    Agent -> ServiceId -> HashedSecret ServiceKey -> ServiceName
    -> ServiceDescription -> ThentosQuery e ()
addService agent sid secret name description = void $
    execT [sql| INSERT INTO services (id, owner, name, description, key)
                VALUES (?, ?, ?, ?, ?)
          |] (sid, agent, name, description, secret)

deleteService :: ServiceId -> ThentosQuery e ()
deleteService sid = do
    deletedCount <- execT [sql| DELETE FROM services
                                WHERE id = ? |] (Only sid)
    case deletedCount of
        0 -> throwError NoSuchService
        1 -> return ()
        _ -> impossible "deleteService: multiple results"

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

startThentosSession :: ThentosSessionToken -> Agent -> Timeout
                                       -> ThentosQuery e ()
startThentosSession tok agent period =
    void $ execT [sql| INSERT INTO user_sessions (token, uid, start, end_, period)
                       VALUES (?, ?, now(), now() + ?, ?)
                 |] (tok, agent, period, period)

endThentosSession :: ThentosSessionToken -> ThentosQuery e ()
endThentosSession tok =
    void $ execT [sql| DELETE FROM user_sessions WHERE token = ?
                 |] (Only tok)

lookupServiceSession :: ServiceSessionToken -> ThentosQuery e (ServiceSessionToken, ServiceSession)
lookupServiceSession token = do
    sessions <- queryT
        [sql| SELECT * FROM WHERE token = ? |] (Only token)
    case sessions of
        []        -> throwError NoSuchServiceSession
        [session] -> return (token, session)
        _         -> impossible "multiple sessions with the same token"

startServiceSession ::
    ThentosSessionToken -> ServiceSessionToken -> ServiceId
    -> Timeout -> ThentosQuery e ()
startServiceSession thentosSessionToken token sid timeout =
    void $ execT [sql| INSERT INTO service_sessions
                        (token, thentos_session_token, start, end_, period, service)
                       VALUES (?, ?, now(), now() + ?, ?, ?) |]
                (token, thentosSessionToken, timeout, timeout, sid)

endServiceSession :: ServiceSessionToken -> ThentosQuery e ()
endServiceSession token = do
    deleted <- execT
        [sql| DELETE FROM service_sessions WHERE token = ? |] (Only token)
    case deleted of
        0 -> throwError NoSuchServiceSession
        1 -> return ()
        _ -> impossible "multiple service sessions with same token"

assertAgent :: Agent -> ThentosQuery e ()
assertAgent = error "src/Thentos/Transaction/Transactions.hs:145"

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

unassignRole :: Agent -> Role -> ThentosQuery e ()
unassignRole agent role = case agent of
    ServiceA _ -> error "unassignRole not implemented for services"
    UserA uid  -> do
        void $ execT [sql| DELETE FROM user_roles WHERE uid = ? AND role = ? |]
                           (uid, role)

agentRoles :: Agent -> ThentosQuery e (Set.Set Role)
agentRoles agent = case agent of
    ServiceA _ -> error "agentRoles not implemented for services"
    UserA uid  -> do
        roles <- queryT [sql| SELECT role FROM user_roles WHERE uid = ? |] (Only uid)
        return $ Set.fromList roles

-- * Garbage collection

garbageCollectThentosSessions :: ThentosQuery e ()
garbageCollectThentosSessions = error "src/Thentos/Transaction/Transactions.hs:161"

garbageCollectServiceSessions :: ThentosQuery e ()
garbageCollectServiceSessions = error "src/Thentos/Transaction/Transactions.hs:169"

garbageCollectUnconfirmedUsers :: Timeout -> ThentosQuery e ()
garbageCollectUnconfirmedUsers timeout = void $ execT [sql|
    DELETE FROM "users" WHERE created < now() - interval '? seconds' AND confirmed = false;
    |] (Only (round timeout :: Integer))

garbageCollectPasswordResetTokens :: Timeout -> ThentosQuery e ()
garbageCollectPasswordResetTokens timeout = void $ execT [sql|
    DELETE FROM "password_reset_tokens" WHERE timestamp < now() - interval '? seconds';
    |] (Only (round timeout :: Integer))

garbageCollectEmailChangeTokens :: Timeout -> ThentosQuery e ()
garbageCollectEmailChangeTokens timeout = void $ execT [sql|
    DELETE FROM "email_change_tokens" WHERE timestamp < now() - interval '? seconds';
    |] (Only (round timeout :: Integer))


impossible :: String -> a
impossible = error
