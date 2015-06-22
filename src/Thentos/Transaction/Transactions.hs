{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE ViewPatterns         #-}

module Thentos.Transaction.Transactions
where

import Control.Arrow (second)
import Control.Lens ((^.), (.~), (%~))
import Control.Monad (forM_, when, unless, void)
import Control.Monad.Reader (ask)
import Control.Monad.State (modify, gets, get)
import Data.AffineSpace ((.+^))
import Data.EitherR (catchT, throwT)
import Data.Functor.Infix ((<$>))
import Data.Set (Set)
import Language.Haskell.TH.Syntax (Name)
import Data.List (foldl')
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.SafeCopy (deriveSafeCopy, base)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Thentos.Transaction.Core
import Thentos.Types


-- * user

freshUserId :: (AsDB db) => ThentosUpdate' db e UserId
freshUserId = polyUpdate $ do
    uid <- gets (^. asDB . dbFreshUserId)
    modify $ dbFreshUserId .~ succ uid
    return uid

assertUser :: (AsDB db) => Maybe UserId -> User -> ThentosQuery db ()
assertUser mUid user = polyQuery $ ask >>= \ db ->
    if | userFacetExists (^. userName)  mUid user db -> throwT UserNameAlreadyExists
       | userFacetExists (^. userEmail) mUid user db -> throwT UserEmailAlreadyExists
       | True -> return ()

userFacetExists :: Eq a => (User -> a) -> Maybe UserId -> User -> DB -> Bool
userFacetExists facet ((/=) -> notOwnUid) user db =
    (facet user `elem`) . map (facet . snd) . filter (notOwnUid . Just . fst) . Map.toList $ db ^. dbUsers

-- | Handle expiry dates: call a transaction that returns a pair of value and creation 'Timestamp',
-- and test timestamp against current time and timeout value.  If the action's value is 'Nothing' or
-- has expired, throw the given error.
withExpiry :: (AsDB db) => Timestamp -> Timeout -> ThentosError
                        -> ThentosUpdate db (Maybe (a, Timestamp))
                        -> ThentosUpdate db a
withExpiry now expiry thentosError = withExpiryT now expiry thentosError id

-- | Like 'withExpiry', but expects actions of the form offered by 'Map.updateLookupWithKey'.
withExpiryU :: (AsDB db) => Timestamp -> Timeout -> ThentosError
                         -> ThentosUpdate db (Maybe (a, Timestamp), m)
                         -> ThentosUpdate db (a, m)
withExpiryU now expiry thentosError = withExpiryT now expiry thentosError f
  where
    f (Just (a, timestamp), m) = Just ((a, m), timestamp)
    f (Nothing, _)             = Nothing

-- | Like 'withExpiry', but takes an extra function for transforming the result value of the action
-- into what we need for the timeout check.
withExpiryT :: (AsDB db) => Timestamp -> Timeout -> ThentosError -> (b -> Maybe (a, Timestamp))
    -> ThentosUpdate db b -> ThentosUpdate db a
withExpiryT now expiry thentosError f action = do
    let isActive crt = fromTimestamp crt .+^ fromTimeout expiry < fromTimestamp now
    mResult <- f <$> action
    case mResult of
        Nothing -> throwT thentosError
        Just (result, tokenCreationTime) -> if isActive tokenCreationTime
            then throwT thentosError
            else return result

trans_allUserIds :: (AsDB db) => ThentosQuery db [UserId]
trans_allUserIds = polyQuery $ Map.keys . (^. dbUsers) <$> ask

trans_lookupUser :: (AsDB db) => UserId -> ThentosQuery db (UserId, User)
trans_lookupUser uid = polyQuery $ ask >>= maybe (throwT NoSuchUser) return . (`pureLookupUser` uid)

pureLookupUser :: DB -> UserId -> Maybe (UserId, User)
pureLookupUser db uid = fmap (uid,) . Map.lookup uid $ db ^. dbUsers

trans_lookupUserByName :: (AsDB db) => UserName -> ThentosQuery db (UserId, User)
trans_lookupUserByName name = polyQuery $ ask >>= maybe (throwT NoSuchUser) return . f
  where
    f :: DB -> Maybe (UserId, User)
    f db = maybeUserId >>= pureLookupUser db
      where maybeUserId = Map.lookup name $ db ^. dbUserIdsByName

trans_lookupUserByEmail :: (AsDB db) => UserEmail -> ThentosQuery db (UserId, User)
trans_lookupUserByEmail email = polyQuery $ ask >>= maybe (throwT NoSuchUser) return . f
  where
    f :: DB -> Maybe (UserId, User)
    f db = maybeUserId >>= pureLookupUser db
      where maybeUserId = Map.lookup email $ db ^. dbUserIdsByEmail

-- | Actually add a new user who already has an ID.
trans_addUserPrim :: (AsDB db) => UserId -> User -> ThentosUpdate db ()
trans_addUserPrim uid user = polyUpdate $ do
    modify $ dbUsers %~ Map.insert uid user
    modify $ dbUserIdsByName %~ Map.insert (user ^. userName) uid
    modify $ dbUserIdsByEmail %~ Map.insert (user ^. userEmail) uid

-- | Add a new user.  Return the new user's 'UserId'.  Call 'assertUser' for name clash exceptions.
trans_addUser :: (AsDB db) => User -> ThentosUpdate db UserId
trans_addUser user = do
    liftThentosQuery $ assertUser Nothing user
    uid <- freshUserId
    trans_addUserPrim uid user
    return uid

-- | Add a list of new users.  This could be inlined in a sequence of transactions, but that would
-- have different rollback behaviour.
trans_addUsers :: (AsDB db) => [User] -> ThentosUpdate db [UserId]
trans_addUsers = mapM trans_addUser

-- | Add a new unconfirmed user (i.e. one whose email address we haven't confirmed yet).  Call
-- 'assertUser' for name clash exceptions.
trans_addUnconfirmedUser :: (AsDB db) => Timestamp -> ConfirmationToken -> User
                                      -> ThentosUpdate db (UserId, ConfirmationToken)
trans_addUnconfirmedUser now token user = polyUpdate $ do
    liftThentosQuery $ assertUser Nothing user
    uid <- freshUserId
    modify $ dbUnconfirmedUsers %~ Map.insert token ((uid, user), now)
    return (uid, token)

trans_finishUserRegistration :: (AsDB db) => Timestamp -> Timeout -> ConfirmationToken
                                          -> ThentosUpdate db UserId
trans_finishUserRegistration now expiry token = polyUpdate $ do
    (uid, user) <- withExpiry now expiry NoSuchPendingUserConfirmation $
        Map.lookup token <$> gets (^. dbUnconfirmedUsers)
    modify $ dbUnconfirmedUsers %~ Map.delete token
    trans_addUserPrim uid user
    return uid

-- | Add a password reset token.  Return the user whose password this token can change.
trans_addPasswordResetToken :: (AsDB db) => Timestamp -> UserEmail -> PasswordResetToken
                                         -> ThentosUpdate db User
trans_addPasswordResetToken timestamp email token = polyUpdate $ do
    (uid, user) <- liftThentosQuery $ trans_lookupUserByEmail email
    modify $ dbPwResetTokens %~ Map.insert token (uid, timestamp)
    return user


-- | Change a password with a given password reset token and remove the token.  Throw an error if
-- the token does not exist or has expired.
trans_resetPassword :: (AsDB db) => Timestamp -> Timeout -> PasswordResetToken
                                 -> HashedSecret UserPass
                                 -> ThentosUpdate db ()
trans_resetPassword now expiry token newPass = polyUpdate $ do
    (uid, toks') <- withExpiryU now expiry NoSuchToken $
        Map.updateLookupWithKey (\ _ _ -> Nothing) token <$> gets (^. dbPwResetTokens)
    modify $ dbPwResetTokens .~ toks'
    (_, user) <- liftThentosQuery $ trans_lookupUser uid
    modify $ dbUsers %~ Map.insert uid (userPassword .~ newPass $ user)

trans_addUserEmailChangeRequest :: (AsDB db) => Timestamp -> UserId -> UserEmail
                                             -> ConfirmationToken
                                             -> ThentosUpdate db ()
trans_addUserEmailChangeRequest timestamp uid email token = polyUpdate $ do
    modify $ dbEmailChangeTokens %~ Map.insert token ((uid, email), timestamp)

-- | Change email with a given token and remove the token.  Throw an error if the token does not
-- exist or has expired.
trans_confirmUserEmailChange :: (AsDB db) => Timestamp -> Timeout -> ConfirmationToken
                                          -> ThentosUpdate db UserId
trans_confirmUserEmailChange now expiry token = polyUpdate $ do
    ((uid, email), toks') <- withExpiryU now expiry NoSuchToken $
        Map.updateLookupWithKey (\ _ _ -> Nothing) token <$> gets (^. dbEmailChangeTokens)
    modify $ dbEmailChangeTokens .~ toks'
    trans_updateUserField uid (UpdateUserFieldEmail email)
    return uid

-- | Look up an email change token. Does not verify that the token is still
-- valid, just retrieves it from the database.
trans_lookupEmailChangeToken :: (AsDB db) => ConfirmationToken -> ThentosQuery db ((UserId, UserEmail), Timestamp)
trans_lookupEmailChangeToken tok = polyQuery $ do
    emailTokens <- (^. dbEmailChangeTokens) <$> ask
    case Map.lookup tok emailTokens of
        Just result -> return result
        Nothing     -> throwT NoSuchToken

data UpdateUserFieldOp =
    UpdateUserFieldName UserName
  | UpdateUserFieldEmail UserEmail
  | UpdateUserFieldInsertService ServiceId ServiceAccount
  | UpdateUserFieldDropService ServiceId
  | UpdateUserFieldPassword (HashedSecret UserPass)
  deriving (Eq)

-- | Turn 'UpdateUserFieldOp' into an actual action.  The boolean states whether 'assertUser' should
-- be run after the update to maintain consistency.
runUpdateUserFieldOp :: UpdateUserFieldOp -> (User -> User, Bool)
runUpdateUserFieldOp (UpdateUserFieldName n)            = (userName .~ n, True)
runUpdateUserFieldOp (UpdateUserFieldEmail e)           = (userEmail .~ e, True)
runUpdateUserFieldOp (UpdateUserFieldInsertService s a) = (userServices %~ Map.insert s a, False)
runUpdateUserFieldOp (UpdateUserFieldDropService sid)   = (userServices %~ f, False)
  where f = Map.filterWithKey (const . (/= sid))
runUpdateUserFieldOp (UpdateUserFieldPassword p)        = (userPassword .~ p, False)

-- | See 'trans_updateUserFields'.
trans_updateUserField :: (AsDB db) => UserId -> UpdateUserFieldOp -> ThentosUpdate db ()
trans_updateUserField uid op = trans_updateUserFields uid [op]

-- | Update existing user.  Throw an error if 'UserId' does not exist or if 'assertUser' is not
-- happy with the requested changes.
trans_updateUserFields :: (AsDB db) => UserId -> [UpdateUserFieldOp] -> ThentosUpdate db ()
trans_updateUserFields uid freeOps = polyUpdate $ do
    let (ops, runCheck) = second or . unzip $ runUpdateUserFieldOp <$> freeOps

    (_, user) <- liftThentosQuery $ trans_lookupUser uid
    let user' = foldl' (flip ($)) user ops
    when runCheck $
        liftThentosQuery $ assertUser (Just uid) user'
    modify $ dbUsers %~ Map.insert uid user'
    when (user ^. userName /= user' ^. userName) $ do
        modify $ dbUserIdsByName %~ Map.insert (user' ^. userName) uid
        modify $ dbUserIdsByName %~ Map.delete (user ^. userName)
    when (user ^. userEmail /= user' ^. userEmail) $ do
        modify $ dbUserIdsByEmail %~ Map.insert (user' ^. userEmail) uid
        modify $ dbUserIdsByEmail %~ Map.delete (user ^. userEmail)

-- | Delete user with given 'UserId'.  Throw an error if user does not exist.
trans_deleteUser :: (AsDB db) => UserId -> ThentosUpdate db ()
trans_deleteUser uid = polyUpdate $ do
    (_, user) <- liftThentosQuery $ trans_lookupUser uid
    forM_ (Set.elems $ user ^. userThentosSessions) trans_endThentosSession
    modify $ dbUsers %~ Map.delete uid
    modify $ dbUserIdsByName %~ Map.delete (user ^. userName)
    modify $ dbUserIdsByEmail %~ Map.delete (user ^. userEmail)


-- * service

trans_allServiceIds :: (AsDB db) => ThentosQuery db [ServiceId]
trans_allServiceIds = polyQuery $ Map.keys . (^. dbServices) <$> ask

trans_lookupService :: (AsDB db) => ServiceId -> ThentosQuery db (ServiceId, Service)
trans_lookupService sid = polyQuery $ ask >>= maybe (throwT NoSuchService) return . f
  where
    f :: DB -> Maybe (ServiceId, Service)
    f db = (sid,) <$> Map.lookup sid (db ^. dbServices)

-- | Add new service.
trans_addService :: (AsDB db) => Agent -> ServiceId -> HashedSecret ServiceKey -> ServiceName
                              -> ServiceDescription -> ThentosUpdate db ()
trans_addService owner sid key name desc = polyUpdate $ do
    let service :: Service
        service = Service key owner Nothing name desc Map.empty
    modify $ dbServices %~ Map.insert sid service

-- | Delete service with given 'ServiceId'.  Throw an error if service does not exist.
trans_deleteService :: (AsDB db) => ServiceId -> ThentosUpdate db ()
trans_deleteService sid = polyUpdate $ do
    (_, service) <- liftThentosQuery $ trans_lookupService sid
    maybe (return ()) trans_endThentosSession (service ^. serviceThentosSession)
    modify $ dbServices %~ Map.delete sid

-- | For a given service and user id, look up all groups the user has in the context of that service
-- from the service's group tree, and collect them into a list.
flattenGroups :: Service -> UserId -> [Group]
flattenGroups ((^. serviceGroups) -> groupMap) = Set.toList . f . GroupU
  where
    memberships :: GroupNode -> Set Group
    memberships g = Map.findWithDefault Set.empty g groupMap

    unionz :: Set (Set Group) -> Set Group
    unionz = Set.fold Set.union Set.empty

    f :: GroupNode -> Set Group
    f g@(GroupU _) =                r g
    f g@(GroupG n) = n `Set.insert` r g

    r :: GroupNode -> Set Group
    r g = unionz $ Set.map (f . GroupG) (memberships g)


-- * thentos and service session

-- | Take a time and a thentos session, and decide whether the time lies between session start and
-- end.
thentosSessionNowActive :: Timestamp -> ThentosSession -> Bool
thentosSessionNowActive now session = (session ^. thSessStart) < now && now < (session ^. thSessEnd)

-- | Lookup session.  If session does not exist or has expired, throw an error.  If it does exist,
-- dump the expiry time and return session with bumped expiry time.
trans_lookupThentosSession :: (AsDB db) => Timestamp -> ThentosSessionToken
                                        -> ThentosUpdate db (ThentosSessionToken, ThentosSession)
trans_lookupThentosSession now tok = polyUpdate $ do
    session <- Map.lookup tok . (^. dbThentosSessions) <$> get
           >>= \case Just s  -> return s
                     Nothing -> throwT NoSuchThentosSession

    unless (thentosSessionNowActive now session) $
        throwT NoSuchThentosSession

    let session' = thSessEnd .~ end' $ session
        end' = Timestamp $ fromTimestamp now .+^ fromTimeout (session ^. thSessExpirePeriod)

    modify $ dbThentosSessions %~ Map.insert tok session'
    return (tok, session')

-- | Start a new thentos session.  Start and end time have to be passed explicitly.  Call
-- 'trans_assertAgent' on session owner.  If the agent is a user, this new session is added to their
-- existing sessions.  If the agent is a service with an existing session, its session is replaced.
trans_startThentosSession :: (AsDB db) => ThentosSessionToken -> Agent -> Timestamp -> Timeout
                                       -> ThentosUpdate db ()
trans_startThentosSession tok owner start expiry = polyUpdate $ do
    let session = ThentosSession owner start end expiry Set.empty
        end = Timestamp $ fromTimestamp start .+^ fromTimeout expiry
    liftThentosQuery $ trans_assertAgent owner
    modify $ dbThentosSessions %~ Map.insert tok session
    updAgent owner
  where
    updAgent :: Agent -> ThentosUpdate DB ()
    updAgent (UserA    uid) = modify $ dbUsers
                                    %~ Map.adjust (userThentosSessions %~ Set.insert tok) uid
    updAgent (ServiceA sid) = modify $ dbServices
                                    %~ Map.adjust (serviceThentosSession .~ Just tok) sid

-- | End thentos session and all associated service sessions.  Call 'trans_assertAgent' on session
-- owner.  If thentos session does not exist or has expired, remove it just the same.
--
-- Always call this transaction if you want to clean up a session (e.g., from a garbage collection
-- transaction).  This way in the future, you can replace this transaction easily by one that does
-- not actually destroy the session, but move it to an archive.
trans_endThentosSession :: (AsDB db) => ThentosSessionToken -> ThentosUpdate db ()
trans_endThentosSession tok = polyUpdate $ do
    mSession <- Map.lookup tok . (^. dbThentosSessions) <$> get
    case mSession of
        Just session -> do
            delThentosSession
            delServiceSessions session
            let agent = session ^. thSessAgent
            liftThentosQuery $ trans_assertAgent agent
            cleanupAgent agent
        Nothing -> return ()
  where
    delThentosSession :: ThentosUpdate DB ()
    delThentosSession = modify $ dbThentosSessions %~ Map.delete tok

    delServiceSessions :: ThentosSession -> ThentosUpdate DB ()
    delServiceSessions session = modify $ dbServiceSessions %~ Map.filterWithKey dropThem
      where
        deadServiceSessions = session ^. thSessServiceSessions
        dropThem t _ = Set.notMember t deadServiceSessions

    cleanupAgent :: Agent -> ThentosUpdate DB ()
    cleanupAgent (UserA    uid) = modify $ dbUsers
                                        %~ Map.adjust (userThentosSessions %~ Set.delete tok) uid
    cleanupAgent (ServiceA sid) = modify $ dbServices
                                        %~ Map.adjust (serviceThentosSession .~ Nothing)      sid


-- | Take a time and a service session, and decide whether the time lies between session start and
-- end.  Does not check the associated thentos session!
serviceSessionNowActive :: Timestamp -> ServiceSession -> Bool
serviceSessionNowActive now session = (session ^. srvSessStart) < now && now < (session ^. srvSessEnd)

-- | Like 'trans_lookupThentosSession', but for 'ServiceSession'.  Bump both service and associated
-- thentos session.  If the service session is still active, but the associated thentos session has
-- expired, update service sessions expiry time to @now@ and throw 'NoSuchThentosSession'.
trans_lookupServiceSession :: (AsDB db) => Timestamp -> ServiceSessionToken
                                        -> ThentosUpdate db (ServiceSessionToken, ServiceSession)
trans_lookupServiceSession now tok = polyUpdate $ do
    session <- Map.lookup tok . (^. dbServiceSessions) <$> get
           >>= \case Just s  -> return s
                     Nothing -> throwT NoSuchServiceSession

    unless (serviceSessionNowActive now session) $
        throwT NoSuchServiceSession

    _ <- catchT (trans_lookupThentosSession now (session ^. srvSessThentosSession)) $
        \ e -> do
            when (e == NoSuchThentosSession) $
                trans_endServiceSession tok
            throwT e

    let session' = srvSessEnd .~ end' $ session
        end' = Timestamp $ fromTimestamp now .+^ fromTimeout (session ^. srvSessExpirePeriod)
    modify $ dbServiceSessions %~ Map.insert tok session'
    return (tok, session')

-- | 'trans_starThentosSession' for service sessions.  Bump associated thentos session.  Throw an
-- error if thentos session lookup fails.  If a service session already exists for the given
-- 'ServiceId', return its token.
trans_startServiceSession :: (AsDB db) => ThentosSessionToken -> ServiceSessionToken -> ServiceId
                                       -> Timestamp -> Timeout -> ThentosUpdate db ()
trans_startServiceSession ttok stok sid start expiry = polyUpdate $ do
    (_, tsession) <- trans_lookupThentosSession start ttok

    (_, user) <- case tsession ^. thSessAgent of
        ServiceA s -> throwT $ NeedUserA ttok s
        UserA u    -> liftThentosQuery $ trans_lookupUser u
    unless (sid `Map.member` (user ^. userServices)) $ throwT NotRegisteredWithService

    let ssession :: ServiceSession
        ssession = ServiceSession sid start end expiry ttok meta
          where
            end = Timestamp $ fromTimestamp start .+^ fromTimeout expiry
            meta = ServiceSessionMetadata $ user ^. userName

    modify $ dbThentosSessions %~ Map.adjust (thSessServiceSessions %~ Set.insert stok) ttok
    modify $ dbServiceSessions %~ Map.insert stok ssession

-- | 'trans_endThentosSession' for service sessions (see there).  If thentos session or service
-- session do not exist or have expired, remove the service session just the same, but never thentos
-- session.
trans_endServiceSession :: (AsDB db) => ServiceSessionToken -> ThentosUpdate db ()
trans_endServiceSession stok = polyUpdate $ do
    mSession <- Map.lookup stok . (^. dbServiceSessions) <$> get
    case mSession of
        Just session -> do
            modify $ dbThentosSessions
                  %~ Map.adjust (thSessServiceSessions %~ Set.delete stok)
                                (session ^. srvSessThentosSession)
            modify $ dbServiceSessions %~ Map.delete stok
        Nothing -> return ()


-- * agent and role

-- | Lookup user or service, resp., and throw an appropriate error if not found.
trans_assertAgent :: (AsDB db) => Agent -> ThentosQuery db ()
trans_assertAgent (UserA    uid) = void $ trans_lookupUser uid
trans_assertAgent (ServiceA sid) = void $ trans_lookupService sid

-- | Extend 'Agent's entry in 'dbRoles' with a new 'Role'.  If 'Role' is already assigned to
-- 'Agent', do nothing.  Call 'trans_assertAgent'.
trans_assignRole :: (AsDB db) => Agent -> Role -> ThentosUpdate db ()
trans_assignRole agent role = polyUpdate $ do
    liftThentosQuery $ trans_assertAgent agent
    let inject = Just . Set.insert role . fromMaybe Set.empty
    modify $ dbRoles %~ Map.alter inject agent

-- | Remove 'Role' from 'Agent's entry in 'dbRoles'.  If 'Role' is not assigned to 'Agent', do
-- nothing.  Call 'trans_assertAgent'.
trans_unassignRole :: (AsDB db) => Agent -> Role -> ThentosUpdate db ()
trans_unassignRole agent role = polyUpdate $ do
    liftThentosQuery $ trans_assertAgent agent
    let exject = fmap $ Set.delete role
    modify $ dbRoles %~ Map.alter exject agent

-- | All 'Role's of an 'Agent'.  If 'Agent' does not exist or has no entry in 'dbRoles', return an
-- empty list.
trans_agentRoles :: (AsDB db) => Agent -> ThentosQuery db (Set.Set Role)
trans_agentRoles agent = polyQuery $ fromMaybe Set.empty . Map.lookup agent . (^. dbRoles) <$> ask


-- * SSO

-- | Add an SSO token to the database
trans_addSsoToken :: AsDB db => SsoToken -> ThentosUpdate db ()
trans_addSsoToken tok = polyUpdate . modify $ dbSsoTokens %~ Set.insert tok

-- | Remove an SSO token from the database. Return True if token existed, False otherwise.
trans_lookupAndRemoveSsoToken :: AsDB db => SsoToken -> ThentosUpdate db Bool
trans_lookupAndRemoveSsoToken tok = polyUpdate $ do
    exists <- Set.member tok . (^. dbSsoTokens) <$> get
    when exists $
        modify $ dbSsoTokens %~ Set.delete tok
    return exists


-- * misc

trans_snapShot :: (AsDB db) => ThentosQuery db DB
trans_snapShot = (^. asDB) <$> ask


-- * garbage collection

-- | Go through 'dbThentosSessions' map and find all expired sessions.
-- Return in 'ThentosQuery'.  (To reduce database locking, call this
-- and then @EndSession@ on all tokens individually.)
trans_garbageCollectThentosSessions :: (AsDB db) => Timestamp
                                                 -> ThentosQuery db [ThentosSessionToken]
trans_garbageCollectThentosSessions now = polyQuery $ do
    sessions <- (^. dbThentosSessions) <$> ask
    return (map fst $ filter (\ (_, s) -> s ^. thSessEnd < now)
                             (Map.assocs sessions))

trans_doGarbageCollectThentosSessions :: (AsDB db) => [ThentosSessionToken] -> ThentosUpdate db ()
trans_doGarbageCollectThentosSessions tokens = forM_ tokens trans_endThentosSession

trans_garbageCollectServiceSessions :: (AsDB db) => Timestamp
                                                 -> ThentosQuery db [ServiceSessionToken]
trans_garbageCollectServiceSessions now = polyQuery $ do
    sessions <- (^. dbServiceSessions) <$> ask
    return (map fst $ filter (\ (_, s) -> s ^. srvSessEnd < now)
                             (Map.assocs sessions))

trans_doGarbageCollectServiceSessions :: (AsDB db) => [ServiceSessionToken] -> ThentosUpdate db ()
trans_doGarbageCollectServiceSessions tokens = forM_ tokens trans_endServiceSession

-- | Remove all expired unconfirmed users from DB.
trans_doGarbageCollectUnconfirmedUsers :: (AsDB db) => Timestamp -> Timeout -> ThentosUpdate db ()
trans_doGarbageCollectUnconfirmedUsers now expiry = polyUpdate $ do
    modify $ dbUnconfirmedUsers %~ removeExpireds now expiry

-- | Remove all expired password reset requests from DB.
trans_doGarbageCollectPasswordResetTokens :: (AsDB db) => Timestamp -> Timeout -> ThentosUpdate db ()
trans_doGarbageCollectPasswordResetTokens now expiry = polyUpdate $ do
    modify $ dbPwResetTokens %~ removeExpireds now expiry

-- | Remove all expired email change requests from DB.
trans_doGarbageCollectEmailChangeTokens :: (AsDB db) => Timestamp -> Timeout -> ThentosUpdate db ()
trans_doGarbageCollectEmailChangeTokens now expiry = polyUpdate $ do
    modify $ dbEmailChangeTokens %~ removeExpireds now expiry

removeExpireds :: Timestamp -> Timeout -> Map k (v, Timestamp) -> Map k (v, Timestamp)
removeExpireds now expiry = Map.filter f
  where f (_, created) = fromTimestamp created .+^ fromTimeout expiry >= fromTimestamp now


-- * wrap-up

$(deriveSafeCopy 0 'base ''UpdateUserFieldOp)

transaction_names :: [Name]
transaction_names =
    [ 'trans_allUserIds
    , 'trans_lookupUser
    , 'trans_lookupUserByName
    , 'trans_lookupUserByEmail
    , 'trans_addUserPrim
    , 'trans_addUser
    , 'trans_addUsers
    , 'trans_addUnconfirmedUser
    , 'trans_finishUserRegistration
    , 'trans_addPasswordResetToken
    , 'trans_resetPassword
    , 'trans_addUserEmailChangeRequest
    , 'trans_lookupEmailChangeToken
    , 'trans_confirmUserEmailChange
    , 'trans_updateUserField
    , 'trans_updateUserFields
    , 'trans_deleteUser
    , 'trans_allServiceIds
    , 'trans_lookupService
    , 'trans_addService
    , 'trans_deleteService
    , 'trans_lookupThentosSession
    , 'trans_startThentosSession
    , 'trans_endThentosSession
    , 'trans_lookupServiceSession
    , 'trans_startServiceSession
    , 'trans_endServiceSession
    , 'trans_assertAgent
    , 'trans_assignRole
    , 'trans_unassignRole
    , 'trans_agentRoles
    , 'trans_addSsoToken
    , 'trans_lookupAndRemoveSsoToken
    , 'trans_snapShot
    , 'trans_garbageCollectThentosSessions
    , 'trans_doGarbageCollectThentosSessions
    , 'trans_garbageCollectServiceSessions
    , 'trans_doGarbageCollectServiceSessions
    , 'trans_doGarbageCollectUnconfirmedUsers
    , 'trans_doGarbageCollectEmailChangeTokens
    , 'trans_doGarbageCollectPasswordResetTokens
    ]
