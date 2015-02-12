{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE DeriveDataTypeable                       #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TemplateHaskell                          #-}
{-# LANGUAGE TupleSections                            #-}
{-# LANGUAGE TypeFamilies                             #-}
{-# LANGUAGE TypeOperators                            #-}
{-# LANGUAGE ViewPatterns                             #-}

{-# OPTIONS -fno-warn-orphans #-}

-- (to enable orphan-instance warning, we could try to make module
-- Core parametric in DB.  but that has its own obvious
-- disadvantages.)

module DB.Trans
  ( AllUserIds(..)
  , LookupUser(..)
  , LookupUserByName(..)
  , AddUser(..)
  , AddUnconfirmedUser(..)
  , FinishUserRegistration(..)
  , AddUsers(..)
  , UpdateUser(..)
  , UpdateUserField(..), UpdateUserFieldOp(..)
  , DeleteUser(..)

  , AllServiceIds(..)
  , LookupService(..)
  , AddService(..)
  , DeleteService(..)

  , AllSessionTokens(..)
  , LookupSessionQ(..)
  , LookupSession(..)
  , StartSession(..)
  , EndSession(..)
  , IsActiveSession(..)
  , IsActiveSessionAndBump(..)
  , IsLoggedIntoService(..)
  , GarbageCollectSessions(..)

  , AssignRole(..)
  , UnassignRole(..)
  , LookupAgentRoles(..)

  , SnapShot(..)

  , pure_lookupUser
  , pure_lookupUserByName
  , pure_lookupService
  , pure_lookupSession, LookupSessionResult(..)
  , pure_lookupAgentRoles

  , emptyDB
  , createCheckpointLoop
  )
where

import Control.Exception (assert)
import Control.Lens ((^.), (.~), (%~))
import Control.Monad.Reader (ask)
import Control.Monad.State (modify, gets, get)
import Data.Acid (Query, Update, makeAcidic)
import Data.AffineSpace ((.+^))
import Data.EitherR (catchT)
import Data.Functor.Infix ((<$>), (<$$>))
import Data.List (nub, find, (\\))
import Data.Maybe (isJust, fromMaybe)
import Data.SafeCopy (deriveSafeCopy, base)
import LIO.DCLabel (ToCNF, (\/), (/\), toCNF)

import qualified Data.Map as Map

import DB.Core
import Types


-- * DB invariants

checkDbInvs :: [DB -> Either DbError ()] -> ThentosQuery ()
checkDbInvs invs = do
    let f []           = returnDb thentosPublic ()
        f (Left e:_)   = throwDb thentosPublic e
        f (Right _:es) = f es

    db <- ask
    f $ fmap ($ db) invs

dbInvUserEmailUnique :: UserId -> User -> DB -> Either DbError ()
dbInvUserEmailUnique uid user db = if nub emails == emails  -- FIXME: O(n^2)
      then Right ()
      else Left UserEmailAlreadyExists
  where
    emails :: [UserEmail]
    emails = (^. userEmail) <$> Map.elems (Map.insert uid user $ db ^. dbUsers)


-- * event functions

emptyDB :: DB
emptyDB = DB Map.empty Map.empty Map.empty Map.empty Map.empty (UserId 0)


-- ** smart accessors

freshUserId :: ThentosUpdate' e UserId
freshUserId = do
    uid <- gets (^. dbFreshUserId)
    modify (dbFreshUserId .~ succ uid)
    return uid


-- ** users

trans_allUserIds :: ThentosQuery [UserId]
trans_allUserIds = ThentosLabeled (RoleAdmin =%% False) . Map.keys . (^. dbUsers) <$> ask

trans_lookupUser :: UserId -> ThentosQuery (UserId, User)
trans_lookupUser uid = (uid,) <$$> do
    let label = RoleAdmin \/ UserA uid =%% False
    db <- ask
    either (throwDb label) (returnDb label) $ pure_lookupUser db uid

pure_lookupUser :: DB -> UserId -> Either DbError User
pure_lookupUser db uid =
    maybe (Left NoSuchUser) Right . Map.lookup uid $ db ^. dbUsers

-- FIXME: this is extremely inefficient, we should have a separate map from
    -- user names to users or user ids
trans_lookupUserByName :: UserName -> ThentosQuery (UserId, User)
trans_lookupUserByName name = do
    mUser <- (`pure_lookupUserByName` name) <$> ask
    let label = case mUser of
          Just (uid, _) -> RoleAdmin \/ UserA uid =%% False
          Nothing       -> RoleAdmin              =%% False
    maybe (throwDb label NoSuchUser) (returnDb label) mUser

pure_lookupUserByName :: DB -> UserName -> Maybe (UserId, User)
pure_lookupUserByName db name =
    find (\ (_, user) -> (user ^. userName == name)) . Map.toList . (^. dbUsers) $ db

-- | Write a new unconfirmed user (i.e. one whose email address we haven't
-- confirmed yet) to DB. Unlike addUser, this operation does not ensure
-- uniqueness of email adresses.
trans_addUnconfirmedUser :: ConfirmationToken -> User -> ThentosUpdate ConfirmationToken
trans_addUnconfirmedUser token user = do
    modify $ dbUnconfirmedUsers %~ Map.insert token user
    returnDb thentosPublic token

trans_finishUserRegistration :: ConfirmationToken -> ThentosUpdate UserId
trans_finishUserRegistration token = do
    users <- gets (^. dbUnconfirmedUsers)
    case Map.lookup token users of
        Nothing -> throwDb thentosPublic NoSuchUser -- FIXME: more specific error
        Just user -> do
            modify $ dbUnconfirmedUsers %~ Map.delete token
            trans_addUser user

-- | Write new user to DB.  Return the fresh user id.
trans_addUser :: User -> ThentosUpdate UserId
trans_addUser user = do
    uid <- freshUserId
    writeUser uid user

-- | Write a list of new users to DB.  Return list of fresh user ids.
-- This is not the most vital part of the backend API, but it allows
-- for testing rollback in error cases.  It will also be a nice
-- example for intersecting authorizations.
trans_addUsers :: [User] -> ThentosUpdate [UserId]
trans_addUsers users = mapM trans_addUser users >>= returnDb thentosPublic . map (\ (ThentosLabeled _ uid) -> uid)

-- | Update existing user in DB.  Throw an error if user id does not
-- exist, or if email address in updated user is already in use by
-- another user.
trans_updateUser :: UserId -> User -> ThentosUpdate ()
trans_updateUser uid user = do
    _ <- liftThentosQuery $ trans_lookupUser uid
    _ <- writeUser uid user
    let label = RoleAdmin \/ UserA uid =%% RoleAdmin /\ UserA uid
    returnDb label ()

data UpdateUserFieldOp =
    UpdateUserFieldName UserName
  | UpdateUserFieldEmail UserEmail
  | UpdateUserFieldAddService ServiceId
  | UpdateUserFieldDropService ServiceId
  deriving (Eq, Show)

trans_updateUserField :: UserId -> UpdateUserFieldOp -> ThentosUpdate ()
trans_updateUserField uid op = do
    let label = RoleAdmin \/ UserA uid =%% RoleAdmin /\ UserA uid
    ThentosLabeled _ user <- ((`pure_lookupUser` uid) <$> get) >>= either (throwDb label) (returnDb label)

    let runOp :: UpdateUserFieldOp -> User -> User
        runOp (UpdateUserFieldName n)          = userName .~ n
        runOp (UpdateUserFieldEmail e)         = userEmail .~ e
        runOp (UpdateUserFieldAddService sid)  = userLogins %~ (sid:)
        runOp (UpdateUserFieldDropService sid) = userLogins %~ filter (/= sid)

    _ <- writeUser uid $ runOp op user

    returnDb label ()

-- | Delete user with given user id.  If user does not exist, throw an
-- error.
trans_deleteUser :: UserId -> ThentosUpdate ()
trans_deleteUser uid = do
    ThentosLabeled _ (_, user :: User) <- liftThentosQuery $ trans_lookupUser uid
    _ <- maybe (returnDb thentosPublic ()) deleteSession (user ^. userSession)
    modify $ dbUsers %~ Map.delete uid
    let label = RoleAdmin \/ UserA uid =%% RoleAdmin /\ UserA uid
    returnDb label ()


-- *** helpers

-- | (db ^. dbUser) must only be modified using this function.
writeUser :: UserId -> User -> ThentosUpdate UserId
writeUser uid user = do
    _ <- liftThentosQuery $ checkDbInvs [dbInvUserEmailUnique uid user]
    modify $ dbUsers %~ Map.insert uid user
    returnDb thentosPublic uid


-- ** services

trans_allServiceIds :: ThentosQuery [ServiceId]
trans_allServiceIds = ThentosLabeled (RoleAdmin =%% False) . Map.keys . (^. dbServices) <$> ask

trans_lookupService :: ServiceId -> ThentosQuery (ServiceId, Service)
trans_lookupService sid = do
    db <- ask
    let label = RoleAdmin =%% False
    maybe (throwDb label NoSuchService) (returnDb label) $
        pure_lookupService db sid

pure_lookupService :: DB -> ServiceId -> Maybe (ServiceId, Service)
pure_lookupService db sid = (sid,) <$> Map.lookup sid (db ^. dbServices)

-- | Write new service to DB.  Service key is generated automatically.
-- Return fresh service id.
trans_addService :: ServiceId -> ServiceKey -> ThentosUpdate (ServiceId, ServiceKey)
trans_addService sid key = do
    let service = Service key Nothing
    modify $ dbServices %~ Map.insert sid service
    returnDb thentosPublic (sid, key)

trans_deleteService :: ServiceId -> ThentosUpdate ()
trans_deleteService sid = do
    ThentosLabeled _ (_, service :: Service) <- liftThentosQuery $ trans_lookupService sid
    _ <- maybe (returnDb thentosPublic ()) deleteSession (service ^. serviceSession)
    modify $ dbServices %~ Map.delete sid
    let label = RoleAdmin \/ ServiceA sid =%% RoleAdmin /\ ServiceA sid
    returnDb label ()


-- ** sessions

-- | Complete list of all active and inactive sessions.  This
-- transaction may go away in the future unless we can think of a good
-- use for it.
trans_allSessionTokens :: ThentosQuery [SessionToken]
trans_allSessionTokens = ThentosLabeled (RoleAdmin =%% False) . Map.keys . (^. dbSessions) <$> ask


data LookupSessionResult =
    LookupSessionUnchanged (SessionToken, Session)
  | LookupSessionBumped (SessionToken, Session)
  | LookupSessionNotThere
  | LookupSessionInactive
  deriving (Eq, Ord, Show, Read)

-- | Lookup session.  Use second arg to only return session if now
-- active.  Use flag in second arg to update end-of-life 'TimeStamp'
-- in result session.
pure_lookupSession :: DB -> Maybe (TimeStamp, Bool) -> SessionToken -> LookupSessionResult
pure_lookupSession db mNow tok =
    let mSession :: Maybe Session = Map.lookup tok $ db ^. dbSessions
    in case (mSession, mNow) of
        (Just session, Just (now, bump)) ->
            if sessionNowActive now session
                then if bump
                    then let newEnd = TimeStamp $ fromTimeStamp now .+^ fromTimeout (session ^. sessionTimeout)
                         in LookupSessionBumped (tok, sessionEnd .~ newEnd $ session)
                    else LookupSessionUnchanged (tok, session)
                else LookupSessionInactive
        (Just session, Nothing) -> LookupSessionUnchanged (tok, session)
        (Nothing, _) -> LookupSessionNotThere


-- | See 'trans_lookupSession'.  The difference is that you cannot
-- bump the session, so there cannot be any changes to the database,
-- so the result monad can be 'ThentosQuery'.
trans_lookupSessionQ :: Maybe TimeStamp -> SessionToken -> ThentosQuery (SessionToken, Session)
trans_lookupSessionQ mNow tok = ask >>= \ db -> do
    let rSession = pure_lookupSession db ((, False) <$> mNow) tok

    case rSession of
        LookupSessionUnchanged (_, session) -> returnDb (RoleAdmin \/ (session ^. sessionAgent) =%% False) (tok, session)
        LookupSessionInactive               -> throwDb (RoleAdmin =%% False) NoSuchSession
        LookupSessionNotThere               -> throwDb (RoleAdmin =%% False) NoSuchSession
        LookupSessionBumped _               -> assert False $ error "trans_lookupSession"


-- | Lookup session.  Use first arg to check if session is now active,
-- and if not, remove it from database.  Use flag in second arg to
-- update end-of-life 'TimeStamp' in result session and database.  If
-- session does not exist, or if active check is enabled and session
-- is inactive, throw 'NoSuchSession'.
--
-- Inactive sessions are not deleted.  See
-- 'trans_GarbageCollectSessions'.
--
-- See 'trans_lookupSessionQ' for a variant of this function in
-- 'ThentosQuery'.
trans_lookupSession :: Maybe (TimeStamp, Bool) -> SessionToken -> ThentosUpdate (SessionToken, Session)
trans_lookupSession mNow tok = lookupSessionTL ([] :: [Bool]) mNow tok


-- | Like 'trans_lookupSession', but accepts an extra list of
-- clearance items.  This is required for service login checks by the
-- service.
--
-- There may be a better way to do this.  See redmine #1688.
lookupSessionTL :: ToCNF a => [a] -> Maybe (TimeStamp, Bool) -> SessionToken
      -> ThentosUpdate (SessionToken, Session)
lookupSessionTL cnfs mNow tok = get >>= \ db -> do
    let rSession = pure_lookupSession db mNow tok

    let label = case rSession of
            LookupSessionUnchanged (_, Session agent _ _ _) -> simpleThentosLabel $ toCNF agent : defaults
            LookupSessionBumped    (_, Session agent _ _ _) -> simpleThentosLabel $ toCNF agent : defaults
            _                                               -> simpleThentosLabel defaults
        defaults = toCNF RoleAdmin : map toCNF cnfs

    case rSession of
        LookupSessionUnchanged (_, session) -> returnDb label (tok, session)
        LookupSessionBumped    (_, session) -> writeSession Nothing tok session >> returnDb label (tok, session)
        LookupSessionInactive               -> throwDb  label NoSuchSession
        LookupSessionNotThere               -> throwDb  label NoSuchSession


-- | Start a new session for user with 'UserId' on service with
-- 'ServiceId'.  Start and end time have to be passed explicitly.
-- Throw error if user or service do not exist.  Otherwise, return
-- session token.  If there is already an active session for this
-- user, return the existing token again, and store new session under
-- it.  If agent is a user, remove all her logins.
trans_startSession :: SessionToken -> Agent -> TimeStamp -> Timeout -> ThentosUpdate SessionToken
trans_startSession freshSessionToken agent start lifetime = do
    let label = agent =%% agent
        session = Session agent start end lifetime
        end = TimeStamp $ fromTimeStamp start .+^ fromTimeout lifetime

    ThentosLabeled _ tok <- fromMaybe freshSessionToken <$$> liftThentosQuery (getSessionFromAgent agent)
    _ <- writeSession (Just $ const []) tok session
    returnDb label tok


-- | End session.  Call can be caused by logout request from user
-- (before end of session life time), by session timeouts, or after
-- its natural life time (by application's own garbage collection).
-- If lookup of session owning user fails, throw an error.
trans_endSession :: SessionToken -> ThentosUpdate ()
trans_endSession tok = do
    mSession :: Maybe Session <- Map.lookup tok . (^. dbSessions) <$> get
    let label = case mSession of
            Just session -> simpleThentosLabel [toCNF RoleAdmin, toCNF (session ^. sessionAgent)]
            Nothing      -> simpleThentosLabel [RoleAdmin]

    _ <- deleteSession tok
    returnDb label ()


-- | Is session token currently active?  (Do not bump either way.)
-- Never throw errors; if session is inaccessible, simple return
-- 'False'.
trans_isActiveSession :: TimeStamp -> SessionToken -> ThentosQuery Bool
trans_isActiveSession now tok = catchT
    (trans_lookupSessionQ (Just now) tok >> returnDb thentosPublic True)
    (const $ returnDb thentosPublic False)


-- | Is session token currently active?  Bump if it is.
trans_isActiveSessionAndBump :: TimeStamp -> SessionToken -> ThentosUpdate Bool
trans_isActiveSessionAndBump now tok = catchT
    (trans_lookupSession (Just (now, True)) tok >> returnDb thentosPublic True)
    (const $ returnDb thentosPublic False)


trans_isLoggedIntoService :: TimeStamp -> SessionToken -> ServiceId -> ThentosUpdate Bool
trans_isLoggedIntoService now tok sid = catchT
    (test >> returnDb thentosPublic True)
    (const $ returnDb thentosPublic False)
  where
    test = do
        ThentosLabeled _ (_, session) <- lookupSessionTL [ServiceA sid] (Just (now, True)) tok
        case session ^. sessionAgent of
            UserA uid  -> do
                ThentosLabeled _ (_, user) <- liftThentosQuery $ trans_lookupUser uid
                if sid `elem` user ^. userLogins
                    then returnDb thentosPublic ()
                    else throwDb thentosPublic OperationNotPossibleInServiceSession
            ServiceA _ -> throwDb thentosPublic NoSuchUser


-- | Go through 'dbSessions' map and find all expired sessions.
-- Return in 'ThentosQuery'.  (To reduce database locking, call this
-- and then @EndSession@ on all service ids individually.)
trans_garbageCollectSessions :: ThentosQuery [SessionToken]
trans_garbageCollectSessions = assert False $ error "trans_GarbageCollectSessions: not implemented"  -- FIXME


-- *** helpers

sessionNowActive :: TimeStamp -> Session -> Bool
sessionNowActive now session = (session ^. sessionStart) < now && now < (session ^. sessionEnd)

-- | Throw error if user or service do not exist.
getSessionFromAgent :: Agent -> ThentosQuery (Maybe SessionToken)
getSessionFromAgent (UserA uid)    = ((^. userSession)    . snd) <$$> trans_lookupUser    uid
getSessionFromAgent (ServiceA sid) = ((^. serviceSession) . snd) <$$> trans_lookupService sid

-- | Write session to database (both in 'dbSessions' and in the
-- 'Agent').  If first arg is given and 'Agent' is 'User', write
-- update service login list, too.
writeSession :: Maybe ([ServiceId] -> [ServiceId]) -> SessionToken -> Session -> ThentosUpdate ()
writeSession (fromMaybe id -> updateLogins) tok session = do
    modify $ dbSessions %~ Map.insert tok session
    modify $ case session ^. sessionAgent of
        UserA    uid -> dbUsers    %~ Map.adjust _updateUser uid
        ServiceA sid -> dbServices %~ Map.adjust _updateService sid
    returnDb thentosPublic ()
  where
    _updateUser :: User -> User
    _updateUser = (userSession .~ Just tok) . (userLogins %~ updateLogins)

    _updateService :: Service -> Service
    _updateService = (serviceSession .~ Just tok)

-- | Write session to database (both in 'dbSessions' and in the
-- 'Agent').  If first arg is given and 'Agent' is 'User', write
-- update service login list, too.
deleteSession :: SessionToken -> ThentosUpdate ()
deleteSession tok = do
    mSession :: Maybe Session <- Map.lookup tok . (^. dbSessions) <$> get
    case (^. sessionAgent) <$> mSession of
        Just (UserA    uid) -> modify $ dbUsers    %~ Map.adjust _updateUser uid
        Just (ServiceA sid) -> modify $ dbServices %~ Map.adjust _updateService sid
        Nothing             -> return ()

    modify $ dbSessions %~ Map.delete tok
    returnDb thentosPublic ()
  where
    _updateUser :: User -> User
    _updateUser = (userSession .~ Nothing) . (userLogins .~ [])

    _updateService :: Service -> Service
    _updateService = serviceSession .~ Nothing


-- * agents and roles

-- | Extend 'Agent's entry in 'dbRoles' with a new 'Role'.  If 'Role'
-- is already assigned to 'Agent', do nothing.  If Agent does not
-- point to an existing entry in user or service table (or such),
-- throw an error.
trans_assignRole :: Agent -> Role -> ThentosUpdate ()
trans_assignRole agent role = liftThentosQuery (assertAgent agent) >> do
    let inject Nothing      = Just [role]
        inject (Just roles) = Just $ role:roles
    modify $ dbRoles %~ Map.alter inject agent
    returnDb (RoleAdmin =%% RoleAdmin) ()

-- | Extend 'Agent's entry in 'dbRoles' with a new 'Role'.  If 'Role'
-- is not assigned to 'Agent', do nothing.  If Agent does not
-- point to an existing entry in user or service table (or such),
-- throw an error.
trans_unassignRole :: Agent -> Role -> ThentosUpdate ()
trans_unassignRole agent role = liftThentosQuery (assertAgent agent) >> do
    let exject Nothing      = Nothing
        exject (Just roles) = Just $ roles \\ [role]
    modify $ dbRoles %~ Map.alter exject agent
    returnDb (RoleAdmin =%% RoleAdmin) ()

-- | All 'Role's of an 'Agent'.  If 'Agent' does not exist or
-- has no entry in 'dbRoles', return an empty list.
trans_lookupAgentRoles :: Agent -> ThentosQuery [Role]
trans_lookupAgentRoles agent = ThentosLabeled (agent \/ RoleAdmin =%% False) .
    (`pure_lookupAgentRoles` agent) <$> ask

pure_lookupAgentRoles :: DB -> Agent -> [Role]
pure_lookupAgentRoles db agent = fromMaybe [] $ Map.lookup agent (db ^. dbRoles)


-- *** helpers

-- | 'assertAgent' is only used by to build transactions, and is not a
-- transaction itself.  Even though it has return type 'ThentosQuery',
-- it does not restrict the label in any way.
--
-- (In the long run, it would be beneficial to make more use of the
-- lio package and the 'LIO' monad in particular, so we could
-- accumulate and intersect labels on the way through complex
-- transactions.)
assertAgent :: Agent -> ThentosQuery ()
assertAgent = f
  where
    f :: Agent -> ThentosQuery ()
    f (UserA    uid) = ask >>= g . Map.lookup uid . (^. dbUsers)
    f (ServiceA sid) = ask >>= g . Map.lookup sid . (^. dbServices)

    g :: Maybe a -> ThentosQuery ()
    g mb = if isJust mb
        then returnDb thentosPublic ()
        else throwDb thentosPublic NoSuchUser


-- ** misc

trans_snapShot :: ThentosQuery DB
trans_snapShot = ask >>= returnDb (RoleAdmin =%% False)


-- * event types

-- FIXME: this section should be entirely constructed by TemplateHaskell

allUserIds :: ThentosClearance -> Query DB (Either DbError [UserId])
allUserIds clearance = runThentosQuery clearance trans_allUserIds

lookupUser :: UserId -> ThentosClearance -> Query DB (Either DbError (UserId, User))
lookupUser uid clearance = runThentosQuery clearance $ trans_lookupUser uid

lookupUserByName :: UserName -> ThentosClearance -> Query DB (Either DbError (UserId, User))
lookupUserByName name clearance = runThentosQuery clearance $ trans_lookupUserByName name

addUnconfirmedUser :: ConfirmationToken -> User -> ThentosClearance -> Update DB (Either DbError ConfirmationToken)
addUnconfirmedUser token user clearance =
    runThentosUpdate clearance $ trans_addUnconfirmedUser token user

finishUserRegistration :: ConfirmationToken -> ThentosClearance -> Update DB (Either DbError UserId)
finishUserRegistration token clearance =
    runThentosUpdate clearance $ trans_finishUserRegistration token

addUser :: User -> ThentosClearance -> Update DB (Either DbError UserId)
addUser user clearance = runThentosUpdate clearance $ trans_addUser user

addUsers :: [User] -> ThentosClearance -> Update DB (Either DbError [UserId])
addUsers users clearance = runThentosUpdate clearance $ trans_addUsers users

updateUser :: UserId -> User -> ThentosClearance -> Update DB (Either DbError ())
updateUser uid user clearance = runThentosUpdate clearance $ trans_updateUser uid user

updateUserField :: UserId -> UpdateUserFieldOp -> ThentosClearance -> Update DB (Either DbError ())
updateUserField uid op clearance = runThentosUpdate clearance $ trans_updateUserField uid op

deleteUser :: UserId -> ThentosClearance -> Update DB (Either DbError ())
deleteUser uid clearance = runThentosUpdate clearance $ trans_deleteUser uid

allServiceIds :: ThentosClearance -> Query DB (Either DbError [ServiceId])
allServiceIds clearance = runThentosQuery clearance trans_allServiceIds

lookupService :: ServiceId -> ThentosClearance -> Query DB (Either DbError (ServiceId, Service))
lookupService sid clearance = runThentosQuery clearance $ trans_lookupService sid

addService :: ServiceId -> ServiceKey -> ThentosClearance -> Update DB (Either DbError (ServiceId, ServiceKey))
addService sid key clearance = runThentosUpdate clearance $ trans_addService sid key

deleteService :: ServiceId -> ThentosClearance -> Update DB (Either DbError ())
deleteService sid clearance = runThentosUpdate clearance $ trans_deleteService sid

allSessionTokens :: ThentosClearance -> Query DB (Either DbError [SessionToken])
allSessionTokens clearance = runThentosQuery clearance trans_allSessionTokens

lookupSessionQ :: Maybe TimeStamp -> SessionToken -> ThentosClearance -> Query DB (Either DbError (SessionToken, Session))
lookupSessionQ mNow tok clearance = runThentosQuery clearance $ trans_lookupSessionQ mNow tok

lookupSession :: Maybe (TimeStamp, Bool) -> SessionToken -> ThentosClearance -> Update DB (Either DbError (SessionToken, Session))
lookupSession mNow tok clearance = runThentosUpdate clearance $ trans_lookupSession mNow tok

startSession :: SessionToken -> Agent -> TimeStamp -> Timeout -> ThentosClearance -> Update DB (Either DbError SessionToken)
startSession tok agent start lifetime clearance = runThentosUpdate clearance $ trans_startSession tok agent start lifetime

endSession :: SessionToken -> ThentosClearance -> Update DB (Either DbError ())
endSession tok clearance = runThentosUpdate clearance $ trans_endSession tok

isActiveSession :: TimeStamp -> SessionToken -> ThentosClearance -> Query DB (Either DbError Bool)
isActiveSession now tok clearance = runThentosQuery clearance $ trans_isActiveSession now tok

isActiveSessionAndBump :: TimeStamp -> SessionToken -> ThentosClearance -> Update DB (Either DbError Bool)
isActiveSessionAndBump now tok clearance = runThentosUpdate clearance $ trans_isActiveSessionAndBump now tok

isLoggedIntoService :: TimeStamp -> SessionToken -> ServiceId -> ThentosClearance -> Update DB (Either DbError Bool)
isLoggedIntoService now tok sid clearance = runThentosUpdate clearance $ trans_isLoggedIntoService now tok sid

garbageCollectSessions :: ThentosClearance -> Query DB (Either DbError [SessionToken])
garbageCollectSessions clearance = runThentosQuery clearance $ trans_garbageCollectSessions

assignRole :: Agent -> Role -> ThentosClearance -> Update DB (Either DbError ())
assignRole agent role clearance = runThentosUpdate clearance $ trans_assignRole agent role

unassignRole :: Agent -> Role -> ThentosClearance -> Update DB (Either DbError ())
unassignRole agent role clearance = runThentosUpdate clearance $ trans_unassignRole agent role

lookupAgentRoles :: Agent -> ThentosClearance -> Query DB (Either DbError [Role])
lookupAgentRoles agent clearance = runThentosQuery clearance $ trans_lookupAgentRoles agent

snapShot :: ThentosClearance -> Query DB (Either DbError DB)
snapShot clearance = runThentosQuery clearance trans_snapShot


$(deriveSafeCopy 0 'base ''UpdateUserFieldOp)

$(makeAcidic ''DB
    [ 'allUserIds
    , 'lookupUser
    , 'lookupUserByName
    , 'addUser
    , 'addUnconfirmedUser
    , 'finishUserRegistration
    , 'addUsers
    , 'updateUser
    , 'updateUserField
    , 'deleteUser

    , 'allServiceIds
    , 'lookupService
    , 'addService
    , 'deleteService

    , 'allSessionTokens
    , 'lookupSessionQ
    , 'lookupSession
    , 'startSession
    , 'endSession
    , 'isActiveSession
    , 'isActiveSessionAndBump
    , 'isLoggedIntoService
    , 'garbageCollectSessions

    , 'assignRole
    , 'unassignRole
    , 'lookupAgentRoles

    , 'snapShot
    ])
