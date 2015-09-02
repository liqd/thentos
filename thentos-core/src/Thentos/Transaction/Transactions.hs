{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Thentos.Transaction.Transactions
where

import qualified Data.Set as Set

import Control.Exception.Lifted (throwIO)
import Data.Monoid (mempty)
import Data.Typeable (Typeable)
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
    users <- queryT [sql| SELECT id, name, password, email
                          FROM users
                          WHERE email = ? |] (Only email)
    case users of
      [(id, name, pwd, email)] -> return (id, User name pwd email mempty mempty)
      []                       -> throwError NoSuchUser
      _                        -> impossible "lookupUserByEmail: multiple users"

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
    mod <- q op
    case mod of
        1 -> return ()
        0 -> throwError NoSuchUser
        _ -> impossible "updateUserField: unique constraint on id violated"
  where
    q op = case op of
        UpdateUserFieldName n ->
            execT [sql| UPDATE users SET name = ? WHERE id = ? |] (n, uid)
        UpdateUserFieldEmail e ->
            execT [sql| UPDATE users SET email = ? WHERE id = ? |] (e, uid)
        UpdateUserFieldPassword p ->
            execT [sql| UPDATE users SET password = ? WHERE id = ? |] (p, uid)
        _ -> error $ "updateUserField op not implemented: " ++ show op

updateUserFields :: UserId -> [UpdateUserFieldOp] -> ThentosQuery e ()
updateUserFields = error "src/Thentos/Transaction/Transactions.hs:102"

deleteUser :: UserId -> ThentosQuery e ()
deleteUser uid
    = execT [sql| DELETE FROM users WHERE id = ? |] (Only uid) >>= \ x -> case x of
      1 -> return ()
      0 -> throwError NoSuchUser
      _ -> impossible "deleteUser: unique constraint on id violated"

allServiceIds :: ThentosQuery e [ServiceId]
allServiceIds = error "src/Thentos/Transaction/Transactions.hs:108"

lookupService :: ServiceId -> ThentosQuery e (ServiceId, Service)
lookupService = error "src/Thentos/Transaction/Transactions.hs:111"

addService ::
    Agent -> ServiceId -> HashedSecret ServiceKey -> ServiceName
    -> ServiceDescription -> ThentosQuery e ()
addService = error "src/Thentos/Transaction/Transactions.hs:116"

deleteService :: ServiceId -> ThentosQuery e ()
deleteService = error "src/Thentos/Transaction/Transactions.hs:119"

lookupThentosSession ::
    Timestamp -> ThentosSessionToken -> ThentosQuery e (ThentosSessionToken, ThentosSession)
lookupThentosSession = error "src/Thentos/Transaction/Transactions.hs:123"

startThentosSession :: ThentosSessionToken -> Agent -> Timestamp -> Timeout
                                       -> ThentosQuery e ()
startThentosSession = error "src/Thentos/Transaction/Transactions.hs:127"

endThentosSession :: ThentosSessionToken -> ThentosQuery e ()
endThentosSession = error "src/Thentos/Transaction/Transactions.hs:130"

lookupServiceSession :: Timestamp -> ServiceSessionToken
                                        -> ThentosQuery e (ServiceSessionToken, ServiceSession)
lookupServiceSession = error "src/Thentos/Transaction/Transactions.hs:134"

startServiceSession ::
    ThentosSessionToken -> ServiceSessionToken -> ServiceId
    -> Timestamp -> Timeout -> ThentosQuery e ()
startServiceSession = error "src/Thentos/Transaction/Transactions.hs:139"

endServiceSession :: ServiceSessionToken -> ThentosQuery e ()
endServiceSession = error "src/Thentos/Transaction/Transactions.hs:142"

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

garbageCollectThentosSessions :: Timestamp -> ThentosQuery e [ThentosSessionToken]
garbageCollectThentosSessions = error "src/Thentos/Transaction/Transactions.hs:157"

doGarbageCollectThentosSessions :: [ThentosSessionToken] -> ThentosQuery e ()
doGarbageCollectThentosSessions = error "src/Thentos/Transaction/Transactions.hs:161"

garbageCollectServiceSessions :: Timestamp -> ThentosQuery e [ServiceSessionToken]
garbageCollectServiceSessions = error "src/Thentos/Transaction/Transactions.hs:165"

doGarbageCollectServiceSessions :: [ServiceSessionToken] -> ThentosQuery e ()
doGarbageCollectServiceSessions = error "src/Thentos/Transaction/Transactions.hs:169"

doGarbageCollectUnconfirmedUsers :: Timeout -> ThentosQuery e ()
doGarbageCollectUnconfirmedUsers timeout = void $ execT [sql|
    DELETE FROM "users" WHERE created < now() - interval '? seconds' AND confirmed = false;
    |] (Only (round timeout :: Integer))

doGarbageCollectPasswordResetTokens :: Timeout -> ThentosQuery e ()
doGarbageCollectPasswordResetTokens timeout = void $ execT [sql|
    DELETE FROM "password_reset_tokens" WHERE timestamp < now() - interval '? seconds';
    |] (Only (round timeout :: Integer))

doGarbageCollectEmailChangeTokens :: Timestamp -> Timeout -> ThentosQuery e ()
doGarbageCollectEmailChangeTokens = error "src/Thentos/Transaction/Transactions.hs:181"

impossible :: String -> a
impossible = error
