module Thentos.Transaction.Transactions
where

import qualified Data.Set as Set

import Thentos.Types
import Thentos.Transaction.Core

flattenGroups :: Service -> UserId -> [Group]
flattenGroups = error "src/Thentos/Transaction/Transactions.hs:10"

freshUserId :: ThentosUpdate UserId
freshUserId = error "src/Thentos/Transaction/Transactions.hs:13"

assertUser :: Maybe UserId -> User -> ThentosQuery ()
assertUser = error "src/Thentos/Transaction/Transactions.hs:16"

assertUserIsNew :: User -> ThentosQuery ()
assertUserIsNew = error "src/Thentos/Transaction/Transactions.hs:19"

type MatchUnconfirmedUserFun = ((UserId, User), Timestamp) -> Maybe UserId

userNameExists :: Maybe UserId -> User -> ThentosQuery ()
userNameExists = error "src/Thentos/Transaction/Transactions.hs:24"

userEmailExists :: Maybe UserId -> User -> ThentosQuery ()
userEmailExists = error "src/Thentos/Transaction/Transactions.hs:27"

userIdExists :: UserId -> ThentosQuery ()
userIdExists = error "src/Thentos/Transaction/Transactions.hs:30"

userFacetExists ::
    ThentosError -> Maybe UserId -> MatchUnconfirmedUserFun -> Maybe UserId -> ThentosQuery ()
userFacetExists err mMatchingConfirmedUid unconfirmedUserMatches mUid = error "src/Thentos/Transaction/Transactions.hs:34"

allUserIds :: ThentosQuery [UserId]
allUserIds = error "src/Thentos/Transaction/Transactions.hs:37"

lookupUser :: UserId -> ThentosQuery (UserId, User)
lookupUser = error "src/Thentos/Transaction/Transactions.hs:40"

lookupUserByName :: UserName -> ThentosQuery (UserId, User)
lookupUserByName = error "src/Thentos/Transaction/Transactions.hs:43"

lookupUserByEmail :: UserEmail -> ThentosQuery (UserId, User)
lookupUserByEmail = error "src/Thentos/Transaction/Transactions.hs:46"

addUserPrim :: UserId -> User -> ThentosUpdate ()
addUserPrim = error "src/Thentos/Transaction/Transactions.hs:49"

addUser :: User -> ThentosUpdate UserId
addUser = error "src/Thentos/Transaction/Transactions.hs:52"

addUsers :: [User] -> ThentosUpdate [UserId]
addUsers = error "src/Thentos/Transaction/Transactions.hs:55"

addUnconfirmedUser ::
    Timestamp -> ConfirmationToken -> User -> ThentosUpdate (UserId, ConfirmationToken)
addUnconfirmedUser = error "src/Thentos/Transaction/Transactions.hs:59"

addUnconfirmedUserWithId ::
    Timestamp -> ConfirmationToken -> User -> UserId -> ThentosUpdate ConfirmationToken
addUnconfirmedUserWithId = error "src/Thentos/Transaction/Transactions.hs:63"

finishUserRegistration ::
    Timestamp -> Timeout -> ConfirmationToken -> ThentosUpdate UserId
finishUserRegistration = error "src/Thentos/Transaction/Transactions.hs:67"

addPasswordResetToken ::
    Timestamp -> UserEmail -> PasswordResetToken -> ThentosUpdate User
addPasswordResetToken = error "src/Thentos/Transaction/Transactions.hs:71"

resetPassword ::
    Timestamp -> Timeout -> PasswordResetToken -> HashedSecret UserPass -> ThentosUpdate ()
resetPassword = error "src/Thentos/Transaction/Transactions.hs:75"

addUserEmailChangeRequest :: Timestamp -> UserId -> UserEmail
                                             -> ConfirmationToken
                                             -> ThentosUpdate ()
addUserEmailChangeRequest = error "src/Thentos/Transaction/Transactions.hs:80"

confirmUserEmailChange ::
    Timestamp -> Timeout -> ConfirmationToken -> ThentosUpdate UserId
confirmUserEmailChange = error "src/Thentos/Transaction/Transactions.hs:84"

lookupEmailChangeToken ::
    ConfirmationToken -> ThentosQuery ((UserId, UserEmail), Timestamp)
lookupEmailChangeToken = error "src/Thentos/Transaction/Transactions.hs:88"

data UpdateUserFieldOp =
    UpdateUserFieldName UserName
  | UpdateUserFieldEmail UserEmail
  | UpdateUserFieldInsertService ServiceId ServiceAccount
  | UpdateUserFieldDropService ServiceId
  | UpdateUserFieldPassword (HashedSecret UserPass)
  deriving (Eq)

updateUserField :: UserId -> UpdateUserFieldOp -> ThentosUpdate ()
updateUserField = error "src/Thentos/Transaction/Transactions.hs:99"

updateUserFields :: UserId -> [UpdateUserFieldOp] -> ThentosUpdate ()
updateUserFields = error "src/Thentos/Transaction/Transactions.hs:102"

deleteUser :: UserId -> ThentosUpdate ()
deleteUser = error "src/Thentos/Transaction/Transactions.hs:105"

allServiceIds :: ThentosQuery [ServiceId]
allServiceIds = error "src/Thentos/Transaction/Transactions.hs:108"

lookupService :: ServiceId -> ThentosQuery (ServiceId, Service)
lookupService = error "src/Thentos/Transaction/Transactions.hs:111"

addService ::
    Agent -> ServiceId -> HashedSecret ServiceKey -> ServiceName
    -> ServiceDescription -> ThentosUpdate ()
addService = error "src/Thentos/Transaction/Transactions.hs:116"

deleteService :: ServiceId -> ThentosUpdate ()
deleteService = error "src/Thentos/Transaction/Transactions.hs:119"

lookupThentosSession ::
    Timestamp -> ThentosSessionToken -> ThentosUpdate (ThentosSessionToken, ThentosSession)
lookupThentosSession = error "src/Thentos/Transaction/Transactions.hs:123"

startThentosSession :: ThentosSessionToken -> Agent -> Timestamp -> Timeout
                                       -> ThentosUpdate ()
startThentosSession = error "src/Thentos/Transaction/Transactions.hs:127"

endThentosSession :: ThentosSessionToken -> ThentosUpdate ()
endThentosSession = error "src/Thentos/Transaction/Transactions.hs:130"

lookupServiceSession :: Timestamp -> ServiceSessionToken
                                        -> ThentosUpdate (ServiceSessionToken, ServiceSession)
lookupServiceSession = error "src/Thentos/Transaction/Transactions.hs:134"

startServiceSession ::
    ThentosSessionToken -> ServiceSessionToken -> ServiceId
    -> Timestamp -> Timeout -> ThentosUpdate ()
startServiceSession = error "src/Thentos/Transaction/Transactions.hs:139"

endServiceSession :: ServiceSessionToken -> ThentosUpdate ()
endServiceSession = error "src/Thentos/Transaction/Transactions.hs:142"

assertAgent :: Agent -> ThentosQuery ()
assertAgent = error "src/Thentos/Transaction/Transactions.hs:145"

assignRole :: Agent -> Role -> ThentosUpdate ()
assignRole = error "src/Thentos/Transaction/Transactions.hs:148"

unassignRole :: Agent -> Role -> ThentosUpdate ()
unassignRole = error "src/Thentos/Transaction/Transactions.hs:151"

agentRoles :: Agent -> ThentosQuery (Set.Set Role)
agentRoles = error "src/Thentos/Transaction/Transactions.hs:154"

garbageCollectThentosSessions :: Timestamp -> ThentosQuery [ThentosSessionToken]
garbageCollectThentosSessions = error "src/Thentos/Transaction/Transactions.hs:157"

doGarbageCollectThentosSessions ::
    [ThentosSessionToken] -> ThentosUpdate ()
doGarbageCollectThentosSessions = error "src/Thentos/Transaction/Transactions.hs:161"

garbageCollectServiceSessions ::
    Timestamp -> ThentosQuery [ServiceSessionToken]
garbageCollectServiceSessions = error "src/Thentos/Transaction/Transactions.hs:165"

doGarbageCollectServiceSessions ::
    [ServiceSessionToken] -> ThentosUpdate ()
doGarbageCollectServiceSessions = error "src/Thentos/Transaction/Transactions.hs:169"

doGarbageCollectUnconfirmedUsers ::
    Timestamp -> Timeout -> ThentosUpdate ()
doGarbageCollectUnconfirmedUsers = error "src/Thentos/Transaction/Transactions.hs:173"

doGarbageCollectPasswordResetTokens ::
    Timestamp -> Timeout -> ThentosUpdate ()
doGarbageCollectPasswordResetTokens = error "src/Thentos/Transaction/Transactions.hs:177"

doGarbageCollectEmailChangeTokens ::
    Timestamp -> Timeout -> ThentosUpdate ()
doGarbageCollectEmailChangeTokens = error "src/Thentos/Transaction/Transactions.hs:181"
