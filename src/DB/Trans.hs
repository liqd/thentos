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
  , DeleteUser(..)

  , AllServiceIds(..)
  , LookupService(..)
  , AddService(..)
  , DeleteService(..)

  , StartSession(..)
  , AllSessionTokens(..)
  , BumpSession(..)
  , LookupSession(..)
  , EndSession(..)
  , IsActiveSession(..)

  , AssignRole(..)
  , UnassignRole(..)
  , LookupAgentRoles(..)

  , SnapShot(..)

  , pure_lookupUserByName
  , pure_lookupService
  , pure_lookupAgentRoles

  , emptyDB
  , createCheckpointLoop
  )
where

import Control.Lens ((^.), (.~), (%~))
import Control.Monad.Reader (ask)
import Control.Monad.State (modify, gets)
import Data.Acid (Query, Update, makeAcidic)
import Data.AffineSpace ((.+^))
import Data.Function (on)
import Data.Functor.Infix ((<$>), (<$$>))
import Data.List (nub, nubBy, find, (\\))
import Data.Maybe (isJust, fromMaybe)
import LIO.DCLabel ((\/), (/\))

import qualified Data.Map as Map

import DB.Core
import Types


-- * DB invariants

checkDbInvs :: [DB -> Either DbError ()] -> ThentosQuery ()
checkDbInvs invs = do
    let f []           = returnDBQ thentosPublic ()
        f (Left e:_)   = throwDBQ thentosPublic e
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
    perhaps :: Maybe User <- Map.lookup uid . (^. dbUsers) <$> ask
    maybe (throwDBQ label NoSuchUser) (returnDBQ label) perhaps

-- FIXME: this is extremely inefficient, we should have a separate map from
    -- user names to users or user ids
trans_lookupUserByName :: UserName -> ThentosQuery (UserId, User)
trans_lookupUserByName name = do
    mUser <- (`pure_lookupUserByName` name) <$> ask
    let label = case mUser of
          Just (uid, _) -> RoleAdmin \/ UserA uid =%% False
          Nothing       -> RoleAdmin              =%% False
    maybe (throwDBQ label NoSuchUser) (returnDBQ label) mUser

pure_lookupUserByName :: DB -> UserName -> Maybe (UserId, User)
pure_lookupUserByName db name =
    find (\ (_, user) -> (user ^. userName == name)) . Map.toList . (^. dbUsers) $ db

-- | Write a new unconfirmed user (i.e. one whose email address we haven't
-- confirmed yet) to DB. Unlike addUser, this operation does not ensure
-- uniqueness of email adresses.
trans_addUnconfirmedUser :: ConfirmationToken -> User -> ThentosUpdate ConfirmationToken
trans_addUnconfirmedUser token user = do
    modify $ dbUnconfirmedUsers %~ Map.insert token user
    returnDBU thentosPublic token

trans_finishUserRegistration :: ConfirmationToken -> ThentosUpdate UserId
trans_finishUserRegistration token = do
    users <- gets (^. dbUnconfirmedUsers)
    case Map.lookup token users of
        Nothing -> throwDBU thentosPublic NoSuchUser -- FIXME: more specific error
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
trans_addUsers users = mapM trans_addUser users >>= returnDBU thentosPublic . map (\ (ThentosLabeled _ uid) -> uid)

-- | Update existing user in DB.  Throw an error if user id does not
-- exist, or if email address in updated user is already in use by
-- another user.
trans_updateUser :: UserId -> User -> ThentosUpdate ()
trans_updateUser uid user = do
    _ <- liftThentosQuery $ trans_lookupUser uid
    _ <- writeUser uid user
    let label = RoleAdmin \/ UserA uid =%% RoleAdmin /\ UserA uid
    returnDBU label ()

-- | Delete user with given user id.  If user does not exist, throw an
-- error.
trans_deleteUser :: UserId -> ThentosUpdate ()
trans_deleteUser uid = do
    _ <- liftThentosQuery $ trans_lookupUser uid
    modify $ dbUsers %~ Map.delete uid
    let label = RoleAdmin \/ UserA uid =%% RoleAdmin /\ UserA uid
    returnDBU label ()


-- *** helpers

-- | (db ^. dbUser) must only be modified using this function.
writeUser :: UserId -> User -> ThentosUpdate UserId
writeUser uid user = do
    _ <- liftThentosQuery $ checkDbInvs [dbInvUserEmailUnique uid user]
    modify $ dbUsers %~ Map.insert uid user
    returnDBU thentosPublic uid


-- ** services

trans_allServiceIds :: ThentosQuery [ServiceId]
trans_allServiceIds = ThentosLabeled (RoleAdmin =%% False) . Map.keys . (^. dbServices) <$> ask

trans_lookupService :: ServiceId -> ThentosQuery (ServiceId, Service)
trans_lookupService sid = do
    db <- ask
    let label = RoleAdmin =%% False
    maybe (throwDBQ label NoSuchService) (returnDBQ label) $
        pure_lookupService db sid

pure_lookupService :: DB -> ServiceId -> Maybe (ServiceId, Service)
pure_lookupService db sid = (sid,) <$> Map.lookup sid (db ^. dbServices)


-- | Write new service to DB.  Service key is generated automatically.
-- Return fresh service id.
trans_addService :: ServiceId -> ServiceKey -> ThentosUpdate (ServiceId, ServiceKey)
trans_addService sid key = do
    let service = Service key
    modify $ dbServices %~ Map.insert sid service
    returnDBU thentosPublic (sid, key)

trans_deleteService :: ServiceId -> ThentosUpdate ()
trans_deleteService sid = do
    _ <- liftThentosQuery $ trans_lookupService sid
    modify $ dbServices %~ Map.delete sid
    let label = RoleAdmin \/ ServiceA sid =%% RoleAdmin /\ ServiceA sid
    returnDBU label ()

-- FIXME: we don't have any api (neither in DB nor here) to manage
-- user's group data.


-- ** sessions

-- | Start a new session for user with 'UserId' on service with
-- 'ServiceId'.  Start and end time have to be passed explicitly.
-- Throw error if user or service do not exist.  Otherwise, return
-- session token.  If there is already an active session for this user
-- and this service, return the existing token again, and set new
-- start and end times.  (This function also collects all old sessions
-- on the same pair of uid, sid, whether active or inactive, from the
-- global and user-local session maps.)
trans_startSession :: SessionToken -> UserId -> ServiceId -> TimeStamp -> Timeout -> ThentosUpdate SessionToken
trans_startSession freshSessionToken uid sid start lifetime = do
    ThentosLabeled _ (_, user) <- liftThentosQuery $ trans_lookupUser uid
    ThentosLabeled _ _         <- liftThentosQuery $ trans_lookupService sid

    let tok = fromMaybe freshSessionToken $ lookup sid (user ^. userSessions)

    let session = Session uid sid start end lifetime
        end = TimeStamp $ fromTimeStamp start .+^ fromTimeout lifetime

    -- add new token to global session map
    modify $ dbSessions %~ Map.insert tok session

    -- new user-local session map
    let uSessions, uSessions' :: [(ServiceId, SessionToken)]
        uSessions = user ^. userSessions
        uSessions' = nubBy ((==) `on` fst) $ (sid, tok) : uSessions
    modify $ dbUsers %~ Map.insert uid (userSessions .~ uSessions' $ user)

    -- drop old token from global session map (noop if no inactive
    -- session existed).
    case lookup sid uSessions of
        Nothing  -> return ()
        Just tok' -> modify $ dbSessions %~ Map.delete tok'

    let label = UserA uid =%% UserA uid
    returnDBU label tok

trans_allSessionTokens :: ThentosQuery [SessionToken]
trans_allSessionTokens = ThentosLabeled (RoleAdmin =%% False) . Map.keys . (^. dbSessions) <$> ask

-- | Update the end time of a session to @now + timeout@ (where
-- @timeout@ is looked up in the session).  If the session does not
-- exist (or is not active), throw an error.  Return token and
-- session.
trans_bumpSession :: TimeStamp -> SessionToken -> ThentosUpdate (SessionToken, Session)
trans_bumpSession now tok = do
    ThentosLabeled _ result@(_, Session uid sid _ _ timeout)
        <- liftThentosQuery $ trans_lookupSession (Just now) tok

    let end' = TimeStamp $ fromTimeStamp now .+^ fromTimeout timeout
    modify $ dbSessions %~ Map.update (Just . (sessionEnd .~ end')) tok

    let label = RoleAdmin \/ ua \/ sa =%% RoleAdmin /\ ua /\ sa
        ua = UserA uid
        sa = ServiceA sid

    returnDBU label result

-- | Find a session from token.  If first arg is 'Nothing', return
-- session and token unconditionally.  If it is a timestamp, check
-- that the timestamp lies between start and end timestamps of the
-- session.  Do not bump the session!  (Call 'BumpSession' instead if
-- you need that.)
trans_lookupSession :: Maybe TimeStamp -> SessionToken -> ThentosQuery (SessionToken, Session)
trans_lookupSession mNow tok = (tok,) <$$> do
    mSession :: Maybe Session <- Map.lookup tok . (^. dbSessions) <$> ask
    let label = case mSession of
          Just (Session uid sid _ _ _) -> RoleAdmin \/ UserA uid \/ ServiceA sid =%% False
          Nothing                      -> RoleAdmin                              =%% False

    case (mNow, mSession) of
        (_,        Nothing)      -> throwDBQ label NoSuchSession
        (Nothing,  Just session) -> returnDBQ label session
        (Just now, Just session) -> if sessionNowActive now session
            then returnDBQ label session
            else throwDBQ label NoSuchSession

-- | End session.  Call can be caused by logout request from user
-- (before end of session life time), by session timeouts, or after
-- its natural life time (by application's own garbage collection).
-- If lookup of session owning user fails, throw an error.
trans_endSession :: SessionToken -> ThentosUpdate ()
trans_endSession tok = do
    ThentosLabeled _ (_, Session uid sid _ _ _) <- liftThentosQuery $ trans_lookupSession Nothing tok
    ThentosLabeled _ (_, user)                  <- liftThentosQuery $ trans_lookupUser uid
    modify $ dbSessions %~ Map.delete tok
    modify $ dbUsers %~ Map.insert uid (userSessions %~ filter (/= (sid, tok)) $ user)

    let label = RoleAdmin \/ UserA uid \/ ServiceA sid =%% RoleAdmin /\ UserA uid /\ ServiceA sid
    returnDBU label ()

-- | Is session token currently active?
trans_isActiveSession :: TimeStamp -> SessionToken -> ThentosQuery Bool
trans_isActiveSession now tok = do
    mSession :: Maybe Session <- Map.lookup tok . (^. dbSessions) <$> ask
    let label = case mSession of
          Just (Session uid' sid' _ _ _) -> RoleAdmin \/ UserA uid' \/ ServiceA sid' =%% False
          Nothing                        -> RoleAdmin                                =%% False
    returnDBQ label $ maybe False (sessionNowActive now) mSession


-- *** helpers

sessionNowActive :: TimeStamp -> Session -> Bool
sessionNowActive now session = (session ^. sessionStart) < now && now < (session ^. sessionEnd)


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
    returnDBU (RoleAdmin =%% RoleAdmin) ()

-- | Extend 'Agent's entry in 'dbRoles' with a new 'Role'.  If 'Role'
-- is not assigned to 'Agent', do nothing.  If Agent does not
-- point to an existing entry in user or service table (or such),
-- throw an error.
trans_unassignRole :: Agent -> Role -> ThentosUpdate ()
trans_unassignRole agent role = liftThentosQuery (assertAgent agent) >> do
    let exject Nothing      = Nothing
        exject (Just roles) = Just $ roles \\ [role]
    modify $ dbRoles %~ Map.alter exject agent
    returnDBU (RoleAdmin =%% RoleAdmin) ()

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
        then returnDBQ thentosPublic ()
        else throwDBQ thentosPublic NoSuchUser


-- ** misc

trans_snapShot :: ThentosQuery DB
trans_snapShot = ask >>= returnDBQ (RoleAdmin =%% False)


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

startSession :: SessionToken -> UserId -> ServiceId -> TimeStamp -> Timeout -> ThentosClearance -> Update DB (Either DbError SessionToken)
startSession tok uid sid start lifetime clearance = runThentosUpdate clearance $ trans_startSession tok uid sid start lifetime

allSessionTokens :: ThentosClearance -> Query DB (Either DbError [SessionToken])
allSessionTokens clearance = runThentosQuery clearance trans_allSessionTokens

bumpSession :: TimeStamp -> SessionToken -> ThentosClearance -> Update DB (Either DbError (SessionToken, Session))
bumpSession now tok clearance = runThentosUpdate clearance $ trans_bumpSession now tok

lookupSession :: Maybe TimeStamp -> SessionToken -> ThentosClearance -> Query DB (Either DbError (SessionToken, Session))
lookupSession mNow tok clearance = runThentosQuery clearance $ trans_lookupSession mNow tok

endSession :: SessionToken -> ThentosClearance -> Update DB (Either DbError ())
endSession tok clearance = runThentosUpdate clearance $ trans_endSession tok

isActiveSession :: TimeStamp -> SessionToken -> ThentosClearance -> Query DB (Either DbError Bool)
isActiveSession now tok clearance = runThentosQuery clearance $ trans_isActiveSession now tok

assignRole :: Agent -> Role -> ThentosClearance -> Update DB (Either DbError ())
assignRole agent role clearance = runThentosUpdate clearance $ trans_assignRole agent role

unassignRole :: Agent -> Role -> ThentosClearance -> Update DB (Either DbError ())
unassignRole agent role clearance = runThentosUpdate clearance $ trans_unassignRole agent role

lookupAgentRoles :: Agent -> ThentosClearance -> Query DB (Either DbError [Role])
lookupAgentRoles agent clearance = runThentosQuery clearance $ trans_lookupAgentRoles agent

snapShot :: ThentosClearance -> Query DB (Either DbError DB)
snapShot clearance = runThentosQuery clearance trans_snapShot


$(makeAcidic ''DB
    [ 'allUserIds
    , 'lookupUser
    , 'lookupUserByName
    , 'addUser
    , 'addUnconfirmedUser
    , 'finishUserRegistration
    , 'addUsers
    , 'updateUser
    , 'deleteUser

    , 'allServiceIds
    , 'lookupService
    , 'addService
    , 'deleteService

    , 'startSession
    , 'allSessionTokens
    , 'bumpSession
    , 'lookupSession
    , 'endSession
    , 'isActiveSession

    , 'assignRole
    , 'unassignRole
    , 'lookupAgentRoles

    , 'snapShot
    ])
