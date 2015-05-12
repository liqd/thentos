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
import Language.Haskell.TH.Syntax (Name)
import Data.List (find, foldl')
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.SafeCopy (deriveSafeCopy, base)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Thentos.Transaction.Core
import Thentos.Types


-- * user

freshUserId :: ThentosUpdate' e UserId
freshUserId = do
    uid <- gets (^. dbFreshUserId)
    modify $ dbFreshUserId .~ succ uid
    return uid

assertUser :: Maybe UserId -> User -> ThentosQuery ()
assertUser mUid user = ask >>= \ db ->
    if | userFacetExists (^. userName)  mUid user db -> throwT UserNameAlreadyExists
       | userFacetExists (^. userEmail) mUid user db -> throwT UserEmailAlreadyExists
       | True -> return ()

userFacetExists :: Eq a => (User -> a) -> Maybe UserId -> User -> DB -> Bool
userFacetExists facet ((/=) -> notOwnUid) user db =
    (facet user `elem`) . map (facet . snd) . filter (notOwnUid . Just . fst) . Map.toList $ db ^. dbUsers

-- | Handle expiry dates: call a transaction that returns a pair of value and creation 'Timestamp',
-- and test timestamp against current time and timeout value.  If the action's value is 'Nothing' or
-- has expired, throw the given error.
withExpiry :: Timestamp -> Timeout -> ThentosError -> ThentosUpdate (Maybe (a, Timestamp)) -> ThentosUpdate a
withExpiry now expiry thentosError = withExpiryT now expiry thentosError id

-- | Like 'withExpiry', but expects actions of the form offered by 'Map.updateLookupWithKey'.
withExpiryU :: Timestamp -> Timeout -> ThentosError -> ThentosUpdate (Maybe (a, Timestamp), m) -> ThentosUpdate (a, m)
withExpiryU now expiry thentosError = withExpiryT now expiry thentosError f
  where
    f (Just (a, timestamp), m) = Just ((a, m), timestamp)
    f (Nothing, _)             = Nothing

-- | Like 'withExpiry', but takes an extra function for transforming the result value of the action
-- into what we need for the timeout check.
withExpiryT :: Timestamp -> Timeout -> ThentosError -> (b -> (Maybe (a, Timestamp))) -> ThentosUpdate b -> ThentosUpdate a
withExpiryT now expiry thentosError f action = do
    mResult <- f <$> action
    case mResult of
        Nothing -> throwT thentosError
        Just (result, tokenCreationTime) -> if fromTimestamp tokenCreationTime .+^ fromTimeout expiry < fromTimestamp now
            then throwT thentosError
            else return result

trans_allUserIds :: ThentosQuery [UserId]
trans_allUserIds = Map.keys . (^. dbUsers) <$> ask

trans_lookupUser :: UserId -> ThentosQuery (UserId, User)
trans_lookupUser uid = ask >>= maybe (throwT NoSuchUser) return . (`pure_lookupUser` uid)

pure_lookupUser :: DB -> UserId -> Maybe (UserId, User)
pure_lookupUser db uid = fmap (uid,) . Map.lookup uid $ db ^. dbUsers

trans_lookupUserByName :: UserName -> ThentosQuery (UserId, User)
trans_lookupUserByName name = ask >>= maybe (throwT NoSuchUser) return . (`pure_lookupUserByName` name)

-- FIXME: this is extremely inefficient, we should have a separate map from user names to user ids.
pure_lookupUserByName :: DB -> UserName -> Maybe (UserId, User)
pure_lookupUserByName db name =
    find (\ (_, user) -> (user ^. userName == name)) . Map.toList . (^. dbUsers) $ db

trans_lookupUserByEmail :: UserEmail -> ThentosQuery (UserId, User)
trans_lookupUserByEmail email = ask >>= maybe (throwT NoSuchUser) return . (`pure_lookupUserByEmail` email)

-- FIXME: same as 'pure_lookupUserByName'.
pure_lookupUserByEmail :: DB -> UserEmail -> Maybe (UserId, User)
pure_lookupUserByEmail db email =
    find (\ (_, user) -> (user ^. userEmail == email)) . Map.toList . (^. dbUsers) $ db

-- | Add a new user.  Return the new user's 'UserId'.  Call 'assertUser' for name clash exceptions.
trans_addUser :: User -> ThentosUpdate UserId
trans_addUser user = do
    liftThentosQuery $ assertUser Nothing user
    uid <- freshUserId
    modify $ dbUsers %~ Map.insert uid user
    return uid

-- | Add a list of new users.  This could be inlined in a sequence of transactions, but that would
-- have different rollback behaviour.
trans_addUsers :: [User] -> ThentosUpdate [UserId]
trans_addUsers = mapM trans_addUser

-- | Add a new unconfirmed user (i.e. one whose email address we haven't confirmed yet).  Call
-- 'assertUser' for name clash exceptions.
trans_addUnconfirmedUser :: Timestamp -> ConfirmationToken -> User -> ThentosUpdate (UserId, ConfirmationToken)
trans_addUnconfirmedUser now token user = do
    liftThentosQuery $ assertUser Nothing user
    uid <- freshUserId
    modify $ dbUnconfirmedUsers %~ Map.insert token ((uid, user), now)
    return (uid, token)

trans_finishUserRegistration :: Timestamp -> Timeout -> ConfirmationToken -> ThentosUpdate UserId
trans_finishUserRegistration now expiry token = do
    (uid, user) <- withExpiry now expiry NoSuchPendingUserConfirmation $
        Map.lookup token <$> gets (^. dbUnconfirmedUsers)
    modify $ dbUnconfirmedUsers %~ Map.delete token
    modify $ dbUsers %~ Map.insert uid user
    return uid

-- | Add a password reset token.  Return the user whose password this token can change.
trans_addPasswordResetToken :: Timestamp -> UserEmail -> PasswordResetToken -> ThentosUpdate User
trans_addPasswordResetToken timestamp email token = do
    db <- get
    case pure_lookupUserByEmail db email of
        Nothing -> throwT NoSuchUser
        Just (uid, user) -> do
            modify $ dbPwResetTokens %~ Map.insert token (uid, timestamp)
            return user

-- | Change a password with a given password reset token and remove the token.  Throw an error if
-- the token does not exist or has expired.
trans_resetPassword :: Timestamp -> Timeout -> PasswordResetToken -> HashedSecret UserPass -> ThentosUpdate ()
trans_resetPassword now expiry token newPass = do
    (uid, toks') <- withExpiryU now expiry NoSuchToken $
        Map.updateLookupWithKey (\ _ _ -> Nothing) token <$> gets (^. dbPwResetTokens)
    modify $ dbPwResetTokens .~ toks'
    (_, user) <- liftThentosQuery $ trans_lookupUser uid
    modify $ dbUsers %~ Map.insert uid (userPassword .~ newPass $ user)

trans_addUserEmailChangeRequest :: Timestamp -> UserId -> UserEmail -> ConfirmationToken -> ThentosUpdate ()
trans_addUserEmailChangeRequest timestamp uid email token = do
    modify $ dbEmailChangeTokens %~ Map.insert token ((uid, email), timestamp)

-- | Change email with a given token and remove the token.  Throw an error if the token does not
-- exist or has expired.
trans_confirmUserEmailChange :: Timestamp -> Timeout -> ConfirmationToken -> ThentosUpdate ()
trans_confirmUserEmailChange now expiry token = do
    ((uid, email), toks') <- withExpiryU now expiry NoSuchToken $
        Map.updateLookupWithKey (\ _ _ -> Nothing) token <$> gets (^. dbEmailChangeTokens)
    modify $ dbEmailChangeTokens .~ toks'
    trans_updateUserField uid (UpdateUserFieldEmail email)


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
runUpdateUserFieldOp (UpdateUserFieldDropService sid)   = (userServices %~ Map.filterWithKey (const . (/= sid)), False)
runUpdateUserFieldOp (UpdateUserFieldPassword p)        = (userPassword .~ p, False)

-- | See 'trans_updateUserFields'.
trans_updateUserField :: UserId -> UpdateUserFieldOp -> ThentosUpdate ()
trans_updateUserField uid op = trans_updateUserFields uid [op]

-- | Update existing user.  Throw an error if 'UserId' does not exist or if 'assertUser' is not
-- happy with the requested changes.
trans_updateUserFields :: UserId -> [UpdateUserFieldOp] -> ThentosUpdate ()
trans_updateUserFields uid freeOps = do
    let (ops, runCheck) = second or . unzip $ runUpdateUserFieldOp <$> freeOps

    (_, user) <- liftThentosQuery $ trans_lookupUser uid
    let user' = foldl' (flip ($)) user ops
    when runCheck $
        liftThentosQuery $ assertUser (Just uid) user'
    modify $ dbUsers %~ Map.insert uid user'


-- | Delete user with given 'UserId'.  Throw an error if user does not exist.
trans_deleteUser :: UserId -> ThentosUpdate ()
trans_deleteUser uid = do
    (_, user) <- liftThentosQuery $ trans_lookupUser uid
    forM_ (Set.elems $ user ^. userThentosSessions) trans_endThentosSession
    modify $ dbUsers %~ Map.delete uid


-- * service

trans_allServiceIds :: ThentosQuery [ServiceId]
trans_allServiceIds = Map.keys . (^. dbServices) <$> ask

trans_lookupService :: ServiceId -> ThentosQuery (ServiceId, Service)
trans_lookupService sid = ask >>= maybe (throwT NoSuchService) (return) . (`pure_lookupService` sid)

pure_lookupService :: DB -> ServiceId -> Maybe (ServiceId, Service)
pure_lookupService db sid = (sid,) <$> Map.lookup sid (db ^. dbServices)

-- | Add new service.
trans_addService :: Agent -> ServiceId -> HashedSecret ServiceKey -> ServiceName -> ServiceDescription -> ThentosUpdate ()
trans_addService owner sid key name desc = do
    let service :: Service
        service = Service key owner Nothing name desc Map.empty
    modify $ dbServices %~ Map.insert sid service

-- | Delete service with given 'ServiceId'.  Throw an error if service does not exist.
trans_deleteService :: ServiceId -> ThentosUpdate ()
trans_deleteService sid = do
    (_, service) <- liftThentosQuery $ trans_lookupService sid
    maybe (return ()) trans_endThentosSession (service ^. serviceThentosSession)
    modify $ dbServices %~ Map.delete sid


-- * thentos and service session

-- | Take a time and a thentos session, and decide whether the time lies between session start and
-- end.
thentosSessionNowActive :: Timestamp -> ThentosSession -> Bool
thentosSessionNowActive now session = (session ^. thSessStart) < now && now < (session ^. thSessEnd)

-- | Lookup session.  If session does not exist or has expired, throw an error.  If it does exist,
-- dump the expiry time and return session with bumped expiry time.
trans_lookupThentosSession :: Timestamp -> ThentosSessionToken -> ThentosUpdate (ThentosSessionToken, ThentosSession)
trans_lookupThentosSession now tok = do
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
trans_startThentosSession :: ThentosSessionToken -> Agent -> Timestamp -> Timeout -> ThentosUpdate ()
trans_startThentosSession tok owner start expiry = do
    let session = ThentosSession owner start end expiry Set.empty
        end = Timestamp $ fromTimestamp start .+^ fromTimeout expiry
    liftThentosQuery $ trans_assertAgent owner
    modify $ dbThentosSessions %~ Map.insert tok session
    updAgent owner
  where
    updAgent :: Agent -> ThentosUpdate ()
    updAgent (UserA    uid) = modify $ dbUsers    %~ Map.adjust (userThentosSessions %~ Set.insert tok) uid
    updAgent (ServiceA sid) = modify $ dbServices %~ Map.adjust (serviceThentosSession .~ Just tok)     sid

-- | End thentos session and all associated service sessions.  Call 'trans_assertAgent' on session
-- owner.  If thentos session does not exist or has expired, remove it just the same.
--
-- Always call this transaction if you want to clean up a session (e.g., from a garbage collection
-- transaction).  This way in the future, you can replace this transaction easily by one that does
-- not actually destroy the session, but move it to an archive.
trans_endThentosSession :: ThentosSessionToken -> ThentosUpdate ()
trans_endThentosSession tok = do
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
    delThentosSession :: ThentosUpdate ()
    delThentosSession = modify $ dbThentosSessions %~ Map.delete tok

    delServiceSessions :: ThentosSession -> ThentosUpdate ()
    delServiceSessions session = modify $ dbServiceSessions %~ Map.filterWithKey dropThem
      where
        deadServiceSessions = session ^. thSessServiceSessions
        dropThem t _ = Set.notMember t deadServiceSessions

    cleanupAgent :: Agent -> ThentosUpdate ()
    cleanupAgent (UserA    uid) = modify $ dbUsers    %~ Map.adjust (userThentosSessions %~ Set.delete tok) uid
    cleanupAgent (ServiceA sid) = modify $ dbServices %~ Map.adjust (serviceThentosSession .~ Nothing)      sid


-- | Take a time and a service session, and decide whether the time lies between session start and
-- end.  Does not check the associated thentos session!
serviceSessionNowActive :: Timestamp -> ServiceSession -> Bool
serviceSessionNowActive now session = (session ^. srvSessStart) < now && now < (session ^. srvSessEnd)

-- | Like 'trans_lookupThentosSession', but for 'ServiceSession'.  Bump both service and associated
-- thentos session.  If the service session is still active, but the associated thentos session has
-- expired, update service sessions expiry time to @now@ and throw 'NoSuchThentosSession'.
trans_lookupServiceSession :: Timestamp -> ServiceSessionToken -> ThentosUpdate (ServiceSessionToken, ServiceSession)
trans_lookupServiceSession now tok = do
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
trans_startServiceSession :: ThentosSessionToken -> ServiceSessionToken -> ServiceId -> Timestamp -> Timeout -> ThentosUpdate ()
trans_startServiceSession ttok stok sid start expiry = do
    (_, tsession) <- trans_lookupThentosSession start ttok

    (_, user) <- case tsession ^. thSessAgent of
        ServiceA s -> throwT $ NeedUserA ttok s
        UserA u    -> liftThentosQuery $ trans_lookupUser u

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
trans_endServiceSession :: ServiceSessionToken -> ThentosUpdate ()
trans_endServiceSession stok = do
    mSession <- Map.lookup stok . (^. dbServiceSessions) <$> get
    case mSession of
        Just session -> do
            modify $ dbThentosSessions %~ Map.adjust (thSessServiceSessions %~ Set.delete stok) (session ^. srvSessThentosSession)
            modify $ dbServiceSessions %~ Map.delete stok
        Nothing -> return ()


-- * agent and role

-- | Lookup user or service, resp., and throw an appropriate error if not found.
trans_assertAgent :: Agent -> ThentosQuery ()
trans_assertAgent (UserA    uid) = void $ trans_lookupUser uid
trans_assertAgent (ServiceA sid) = void $ trans_lookupService sid

-- | Extend 'Agent's entry in 'dbRoles' with a new 'Role'.  If 'Role' is already assigned to
-- 'Agent', do nothing.  Call 'trans_assertAgent'.
trans_assignRole :: Agent -> Role -> ThentosUpdate ()
trans_assignRole agent role = do
    liftThentosQuery $ trans_assertAgent agent
    let inject = Just . (Set.insert role) . fromMaybe Set.empty
    modify $ dbRoles %~ Map.alter inject agent

-- | Remove 'Role' from 'Agent's entry in 'dbRoles'.  If 'Role' is not assigned to 'Agent', do
-- nothing.  Call 'trans_assertAgent'.
trans_unassignRole :: Agent -> Role -> ThentosUpdate ()
trans_unassignRole agent role = do
    liftThentosQuery $ trans_assertAgent agent
    let exject = fmap $ Set.delete role
    modify $ dbRoles %~ Map.alter exject agent

-- | All 'Role's of an 'Agent'.  If 'Agent' does not exist or has no entry in 'dbRoles', return an
-- empty list.
trans_agentRoles :: Agent -> ThentosQuery (Set.Set Role)
trans_agentRoles agent = fromMaybe Set.empty . Map.lookup agent . (^. dbRoles) <$> ask


-- * misc

trans_snapShot :: ThentosQuery DB
trans_snapShot = ask


-- * garbage collection

-- | Go through 'dbThentosSessions' map and find all expired sessions.
-- Return in 'ThentosQuery'.  (To reduce database locking, call this
-- and then @EndSession@ on all tokens individually.)
trans_garbageCollectThentosSessions :: Timestamp -> ThentosQuery [ThentosSessionToken]
trans_garbageCollectThentosSessions now = do
    sessions <- (^. dbThentosSessions) <$> ask
    return (map fst $ filter (\ (_, s) -> s ^. thSessEnd < now)
                             (Map.assocs sessions))

trans_doGarbageCollectThentosSessions :: [ThentosSessionToken] -> ThentosUpdate ()
trans_doGarbageCollectThentosSessions tokens = forM_ tokens trans_endThentosSession

trans_garbageCollectServiceSessions :: Timestamp -> ThentosQuery [ServiceSessionToken]
trans_garbageCollectServiceSessions now = do
    sessions <- (^. dbServiceSessions) <$> ask
    return (map fst $ filter (\ (_, s) -> s ^. srvSessEnd < now)
                             (Map.assocs sessions))

trans_doGarbageCollectServiceSessions :: [ServiceSessionToken] -> ThentosUpdate ()
trans_doGarbageCollectServiceSessions tokens = forM_ tokens trans_endServiceSession

-- | Remove all expired unconfirmed users from DB.
trans_doGarbageCollectUnconfirmedUsers :: Timestamp -> Timeout -> ThentosUpdate ()
trans_doGarbageCollectUnconfirmedUsers now expiry = do
    modify $ dbUnconfirmedUsers %~ removeExpireds now expiry

-- | Remove all expired password reset requests from DB.
trans_doGarbageCollectPasswordResetTokens :: Timestamp -> Timeout -> ThentosUpdate ()
trans_doGarbageCollectPasswordResetTokens now expiry = do
    modify $ dbPwResetTokens %~ removeExpireds now expiry

-- | Remove all expired email change requests from DB.
trans_doGarbageCollectEmailChangeTokens :: Timestamp -> Timeout -> ThentosUpdate ()
trans_doGarbageCollectEmailChangeTokens now expiry = do
    modify $ dbEmailChangeTokens %~ removeExpireds now expiry

removeExpireds :: Timestamp -> Timeout -> Map k (v, Timestamp) -> Map k (v, Timestamp)
removeExpireds now expiry = Map.filter (\ (_, created) -> fromTimestamp created .+^ fromTimeout expiry >= fromTimestamp now)


-- * wrap-up

$(deriveSafeCopy 0 'base ''UpdateUserFieldOp)

transaction_names :: [Name]
transaction_names = 
    [ 'trans_allUserIds
    , 'trans_lookupUser
    , 'trans_lookupUserByName
    , 'trans_lookupUserByEmail
    , 'trans_addUser
    , 'trans_addUsers
    , 'trans_addUnconfirmedUser
    , 'trans_finishUserRegistration
    , 'trans_addPasswordResetToken
    , 'trans_resetPassword
    , 'trans_addUserEmailChangeRequest
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
    , 'trans_snapShot
    , 'trans_garbageCollectThentosSessions
    , 'trans_doGarbageCollectThentosSessions
    , 'trans_garbageCollectServiceSessions
    , 'trans_doGarbageCollectServiceSessions
    , 'trans_doGarbageCollectUnconfirmedUsers
    , 'trans_doGarbageCollectEmailChangeTokens
    , 'trans_doGarbageCollectPasswordResetTokens
    ]
