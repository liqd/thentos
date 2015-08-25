{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Thentos.Transaction.Transactions
where

import qualified Data.Set as Set

import Data.Monoid (mempty)
import Control.Lens ((^.))
import Control.Monad.Except (throwError)
import Control.Exception (throwIO)
import Database.PostgreSQL.Simple       (Only(..), query)
import Database.PostgreSQL.Simple.Errors (ConstraintViolation(UniqueViolation))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import Thentos.Types
import Thentos.Transaction.Core

flattenGroups :: Service -> UserId -> [Group]
flattenGroups = error "src/Thentos/Transaction/Transactions.hs:10"

freshUserId :: ThentosQuery e UserId
freshUserId = error "src/Thentos/Transaction/Transactions.hs:13"

assertUser :: Maybe UserId -> User -> ThentosQuery e ()
assertUser = error "src/Thentos/Transaction/Transactions.hs:16"

assertUserIsNew :: User -> ThentosQuery e ()
assertUserIsNew = error "src/Thentos/Transaction/Transactions.hs:19"

type MatchUnconfirmedUserFun = ((UserId, User), Timestamp) -> Maybe UserId

userNameExists :: Maybe UserId -> User -> ThentosQuery e ()
userNameExists = error "src/Thentos/Transaction/Transactions.hs:24"

userEmailExists :: Maybe UserId -> User -> ThentosQuery e ()
userEmailExists = error "src/Thentos/Transaction/Transactions.hs:27"

userIdExists :: UserId -> ThentosQuery e ()
userIdExists = error "src/Thentos/Transaction/Transactions.hs:30"

userFacetExists ::
    ThentosError e -> Maybe UserId -> MatchUnconfirmedUserFun -> Maybe UserId -> ThentosQuery e ()
userFacetExists err mMatchingConfirmedUid unconfirmedUserMatches mUid = error "src/Thentos/Transaction/Transactions.hs:34"

allUserIds :: ThentosQuery e [UserId]
allUserIds = error "src/Thentos/Transaction/Transactions.hs:37"

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
      [(id, name, pwd, email)] -> return (id, User name pwd email mempty mempty)
      []                       -> throwError NoSuchUser
      _                        -> impossible "lookupUserByName: multiple users"


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
addUserPrim uid user = execT [sql| INSERT INTO users (id, name, password, email)
                                   VALUES (?, ?, ?, ?) |] ( uid
                                                          , user ^. userName
                                                          , user ^. userPassword
                                                          , user ^. userEmail
                                                          )

addUser :: User -> ThentosQuery e UserId
addUser = error "src/Thentos/Transaction/Transactions.hs:52"

addUsers :: [User] -> ThentosQuery e [UserId]
addUsers = error "src/Thentos/Transaction/Transactions.hs:55"

addUnconfirmedUser ::
    Timestamp -> ConfirmationToken -> User -> ThentosQuery e (UserId, ConfirmationToken)
addUnconfirmedUser = error "src/Thentos/Transaction/Transactions.hs:59"

addUnconfirmedUserWithId ::
    Timestamp -> ConfirmationToken -> User -> UserId -> ThentosQuery e ConfirmationToken
addUnconfirmedUserWithId = error "src/Thentos/Transaction/Transactions.hs:63"

finishUserRegistration ::
    Timestamp -> Timeout -> ConfirmationToken -> ThentosQuery e UserId
finishUserRegistration = error "src/Thentos/Transaction/Transactions.hs:67"

finishUserRegistrationById ::
    Timestamp -> Timeout -> UserId -> ThentosQuery e ()
finishUserRegistrationById = error "finishUserRegistrationById"

addPasswordResetToken ::
    Timestamp -> UserEmail -> PasswordResetToken -> ThentosQuery e User
addPasswordResetToken = error "src/Thentos/Transaction/Transactions.hs:71"

resetPassword ::
    Timestamp -> Timeout -> PasswordResetToken -> HashedSecret UserPass -> ThentosQuery e ()
resetPassword = error "src/Thentos/Transaction/Transactions.hs:75"

addUserEmailChangeRequest :: Timestamp -> UserId -> UserEmail
                                             -> ConfirmationToken
                                             -> ThentosQuery e ()
addUserEmailChangeRequest = error "src/Thentos/Transaction/Transactions.hs:80"

confirmUserEmailChange ::
    Timestamp -> Timeout -> ConfirmationToken -> ThentosQuery e UserId
confirmUserEmailChange = error "src/Thentos/Transaction/Transactions.hs:84"

lookupEmailChangeToken ::
    ConfirmationToken -> ThentosQuery e ((UserId, UserEmail), Timestamp)
lookupEmailChangeToken = error "src/Thentos/Transaction/Transactions.hs:88"

data UpdateUserFieldOp =
    UpdateUserFieldName UserName
  | UpdateUserFieldEmail UserEmail
  | UpdateUserFieldInsertService ServiceId ServiceAccount
  | UpdateUserFieldDropService ServiceId
  | UpdateUserFieldPassword (HashedSecret UserPass)
  deriving (Eq)

updateUserField :: UserId -> UpdateUserFieldOp -> ThentosQuery e ()
updateUserField = error "src/Thentos/Transaction/Transactions.hs:99"

updateUserFields :: UserId -> [UpdateUserFieldOp] -> ThentosQuery e ()
updateUserFields = error "src/Thentos/Transaction/Transactions.hs:102"

deleteUser :: UserId -> ThentosQuery e ()
deleteUser = error "src/Thentos/Transaction/Transactions.hs:105"

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
assignRole = error "src/Thentos/Transaction/Transactions.hs:148"

unassignRole :: Agent -> Role -> ThentosQuery e ()
unassignRole = error "src/Thentos/Transaction/Transactions.hs:151"

agentRoles :: Agent -> ThentosQuery e (Set.Set Role)
agentRoles = error "src/Thentos/Transaction/Transactions.hs:154"

garbageCollectThentosSessions :: Timestamp -> ThentosQuery e [ThentosSessionToken]
garbageCollectThentosSessions = error "src/Thentos/Transaction/Transactions.hs:157"

doGarbageCollectThentosSessions ::
    [ThentosSessionToken] -> ThentosQuery e ()
doGarbageCollectThentosSessions = error "src/Thentos/Transaction/Transactions.hs:161"

garbageCollectServiceSessions ::
    Timestamp -> ThentosQuery e [ServiceSessionToken]
garbageCollectServiceSessions = error "src/Thentos/Transaction/Transactions.hs:165"

doGarbageCollectServiceSessions ::
    [ServiceSessionToken] -> ThentosQuery e ()
doGarbageCollectServiceSessions = error "src/Thentos/Transaction/Transactions.hs:169"

doGarbageCollectUnconfirmedUsers ::
    Timestamp -> Timeout -> ThentosQuery e ()
doGarbageCollectUnconfirmedUsers = error "src/Thentos/Transaction/Transactions.hs:173"

doGarbageCollectPasswordResetTokens ::
    Timestamp -> Timeout -> ThentosQuery e ()
doGarbageCollectPasswordResetTokens = error "src/Thentos/Transaction/Transactions.hs:177"

doGarbageCollectEmailChangeTokens ::
    Timestamp -> Timeout -> ThentosQuery e ()
doGarbageCollectEmailChangeTokens = error "src/Thentos/Transaction/Transactions.hs:181"

impossible :: String -> a
impossible = error
