module Thentos.Transaction.Transactions
where

import qualified Data.Set as Set

import Thentos.Types
import Thentos.Transaction.Core

flattenGroups :: Service -> UserId -> [Group]
flattenGroups = undefined

freshUserId :: ThentosUpdate UserId
freshUserId = undefined

assertUser :: Maybe UserId -> User -> ThentosQuery ()
assertUser = undefined

assertUserIsNew :: User -> ThentosQuery ()
assertUserIsNew = undefined

type MatchUnconfirmedUserFun = ((UserId, User), Timestamp) -> Maybe UserId

userNameExists :: Maybe UserId -> User -> ThentosQuery ()
userNameExists = undefined

userEmailExists :: Maybe UserId -> User -> ThentosQuery ()
userEmailExists = undefined

userIdExists :: UserId -> ThentosQuery ()
userIdExists = undefined

userFacetExists ::
    ThentosError -> Maybe UserId -> MatchUnconfirmedUserFun -> Maybe UserId -> ThentosQuery ()
userFacetExists err mMatchingConfirmedUid unconfirmedUserMatches mUid = undefined

allUserIds :: ThentosQuery [UserId]
allUserIds = undefined

lookupUser :: UserId -> ThentosQuery (UserId, User)
lookupUser = undefined

lookupUserByName :: UserName -> ThentosQuery (UserId, User)
lookupUserByName = undefined

lookupUserByEmail :: UserEmail -> ThentosQuery (UserId, User)
lookupUserByEmail = undefined

addUserPrim :: UserId -> User -> ThentosUpdate ()
addUserPrim = undefined

addUser :: User -> ThentosUpdate UserId
addUser = undefined

addUsers :: [User] -> ThentosUpdate [UserId]
addUsers = undefined

addUnconfirmedUser ::
    Timestamp -> ConfirmationToken -> User -> ThentosUpdate (UserId, ConfirmationToken)
addUnconfirmedUser = undefined

addUnconfirmedUserWithId ::
    Timestamp -> ConfirmationToken -> User -> UserId -> ThentosUpdate ConfirmationToken
addUnconfirmedUserWithId = undefined

finishUserRegistration ::
    Timestamp -> Timeout -> ConfirmationToken -> ThentosUpdate UserId
finishUserRegistration = undefined

addPasswordResetToken ::
    Timestamp -> UserEmail -> PasswordResetToken -> ThentosUpdate User
addPasswordResetToken = undefined

resetPassword ::
    Timestamp -> Timeout -> PasswordResetToken -> HashedSecret UserPass -> ThentosUpdate ()
resetPassword = undefined

addUserEmailChangeRequest :: Timestamp -> UserId -> UserEmail
                                             -> ConfirmationToken
                                             -> ThentosUpdate ()
addUserEmailChangeRequest = undefined

confirmUserEmailChange ::
    Timestamp -> Timeout -> ConfirmationToken -> ThentosUpdate UserId
confirmUserEmailChange = undefined

lookupEmailChangeToken ::
    ConfirmationToken -> ThentosQuery ((UserId, UserEmail), Timestamp)
lookupEmailChangeToken = undefined

data UpdateUserFieldOp =
    UpdateUserFieldName UserName
  | UpdateUserFieldEmail UserEmail
  | UpdateUserFieldInsertService ServiceId ServiceAccount
  | UpdateUserFieldDropService ServiceId
  | UpdateUserFieldPassword (HashedSecret UserPass)
  deriving (Eq)

updateUserField :: UserId -> UpdateUserFieldOp -> ThentosUpdate ()
updateUserField = undefined

updateUserFields :: UserId -> [UpdateUserFieldOp] -> ThentosUpdate ()
updateUserFields = undefined

deleteUser :: UserId -> ThentosUpdate ()
deleteUser = undefined

allServiceIds :: ThentosQuery [ServiceId]
allServiceIds = undefined

lookupService :: ServiceId -> ThentosQuery (ServiceId, Service)
lookupService = undefined

addService ::
    Agent -> ServiceId -> HashedSecret ServiceKey -> ServiceName
    -> ServiceDescription -> ThentosUpdate ()
addService = undefined

deleteService :: ServiceId -> ThentosUpdate ()
deleteService = undefined

lookupThentosSession ::
    Timestamp -> ThentosSessionToken -> ThentosUpdate (ThentosSessionToken, ThentosSession)
lookupThentosSession = undefined

startThentosSession :: ThentosSessionToken -> Agent -> Timestamp -> Timeout
                                       -> ThentosUpdate ()
startThentosSession = undefined

endThentosSession :: ThentosSessionToken -> ThentosUpdate ()
endThentosSession = undefined

lookupServiceSession :: Timestamp -> ServiceSessionToken
                                        -> ThentosUpdate (ServiceSessionToken, ServiceSession)
lookupServiceSession = undefined

startServiceSession ::
    ThentosSessionToken -> ServiceSessionToken -> ServiceId
    -> Timestamp -> Timeout -> ThentosUpdate ()
startServiceSession = undefined

endServiceSession :: ServiceSessionToken -> ThentosUpdate ()
endServiceSession = undefined

assertAgent :: Agent -> ThentosQuery ()
assertAgent = undefined

assignRole :: Agent -> Role -> ThentosUpdate ()
assignRole = undefined

unassignRole :: Agent -> Role -> ThentosUpdate ()
unassignRole = undefined

agentRoles :: Agent -> ThentosQuery (Set.Set Role)
agentRoles = undefined

garbageCollectThentosSessions :: Timestamp -> ThentosQuery [ThentosSessionToken]
garbageCollectThentosSessions = undefined

doGarbageCollectThentosSessions ::
    [ThentosSessionToken] -> ThentosUpdate ()
doGarbageCollectThentosSessions = undefined

garbageCollectServiceSessions ::
    Timestamp -> ThentosQuery [ServiceSessionToken]
garbageCollectServiceSessions = undefined

doGarbageCollectServiceSessions ::
    [ServiceSessionToken] -> ThentosUpdate ()
doGarbageCollectServiceSessions = undefined

doGarbageCollectUnconfirmedUsers ::
    Timestamp -> Timeout -> ThentosUpdate ()
doGarbageCollectUnconfirmedUsers = undefined

doGarbageCollectPasswordResetTokens ::
    Timestamp -> Timeout -> ThentosUpdate ()
doGarbageCollectPasswordResetTokens = undefined

doGarbageCollectEmailChangeTokens ::
    Timestamp -> Timeout -> ThentosUpdate ()
doGarbageCollectEmailChangeTokens = undefined
