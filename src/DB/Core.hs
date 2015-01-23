{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE DeriveDataTypeable                       #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TemplateHaskell                          #-}
{-# LANGUAGE TupleSections                            #-}
{-# LANGUAGE TypeFamilies                             #-}
{-# LANGUAGE TypeOperators                            #-}
{-# LANGUAGE ViewPatterns                             #-}

{-# OPTIONS  #-}

module DB.Core
  ( AllUserIDs(..)
  , LookupUser(..)
  , LookupUserByName(..)
  , AddUser(..)
  , AddUsers(..)
  , UpdateUser(..)
  , DeleteUser(..)

  , AllServiceIDs(..)
  , LookupService(..)
  , AddService(..)
  , DeleteService(..)

  , StartSession(..)
  , AllSessionTokens(..)
  , LookupSession(..)
  , EndSession(..)
  , IsActiveSession(..)

  , emptyDB
  , createCheckpointLoop
  )
where

import Control.Concurrent (threadDelay, forkIO, ThreadId)
import Control.Lens ((^.), (.~), (%~))
import Control.Monad.Reader (ask)
import Control.Monad.State (modify, state, gets, get)
import Data.Acid (AcidState, Query, Update, createCheckpoint, makeAcidic)
import Data.Either (isLeft)
import Data.List (nub, find)
import Data.Functor.Infix ((<$>), (<$$>))
import Data.Maybe (isJust)
import Data.String.Conversions (cs, ST, (<>))
import LIO.DCLabel (dcPublic)

import qualified Codec.Binary.Base32 as Base32
import qualified Crypto.Hash.SHA3 as Hash
import qualified Data.Map as Map

import Types
import DB.Error
import DB.Protect


-- * DB invariants

checkDbInvs :: [DB -> Either DbError ()] -> ThentosQuery ()
checkDbInvs invs = do
    db <- ask
    case filter isLeft $ map ($ db) invs of
      (Left e:_) -> throwDBQ dcPublic e
      [] -> returnDBQ dcPublic ()

dbInvUserEmailUnique :: UserId -> User -> DB -> Either DbError ()
dbInvUserEmailUnique uid user db = if nub emails == emails  -- FIXME: O(n^2)
      then Right ()
      else Left UserEmailAlreadyExists
  where
    emails :: [UserEmail]
    emails = map (^. userEmail) $ Map.elems (Map.insert uid user $ db ^. dbUsers)


-- * event functions

emptyDB :: DB
emptyDB = DB Map.empty Map.empty Map.empty Map.empty (UserId 0) ""

freshUserID :: ThentosClearance -> Update DB (Either DbError UserId)
freshUserID clearance = runThentosUpdate clearance _freshUserID

_freshUserID :: ThentosUpdate UserId
_freshUserID = do
    uid <- gets (^. dbFreshUserId)
    when' (uid == maxBound) $
        throwDBU dcPublic UidOverflow
    modify (dbFreshUserId .~ succ uid)
    returnDBU dcPublic uid

_freshServiceID :: ThentosUpdate ServiceId
_freshServiceID = ServiceId <$$> _freshNonce

_freshServiceKey :: ThentosUpdate ServiceKey
_freshServiceKey = ServiceKey <$$> _freshNonce

_freshSessionToken :: ThentosUpdate SessionToken
_freshSessionToken = SessionToken <$$> _freshNonce

-- | this makes the impression of a cryptographic function, but there
-- is no adversary model and no promise of security.  just yield
-- seemingly random service ids, and update randomness in `DB`.
_freshNonce :: ThentosUpdate ST
_freshNonce = state $ \ db ->
  let r   = db ^. dbRandomness
      r'  = Hash.hash 512 r
      db' = dbRandomness .~ r' $ db
      sid = cs . Base32.encode . Hash.hash 512 $ "_" <> r
  in (thentosLabeledPublic sid, db')


-- ** users

allUserIDs :: ThentosClearance -> Query DB (Either DbError [UserId])
allUserIDs clearance = runThentosQuery clearance _allUserIDs

_allUserIDs :: ThentosQuery [UserId]
_allUserIDs = thentosLabeledPublic . Map.keys . (^. dbUsers) <$> ask

lookupUser :: UserId -> ThentosClearance -> Query DB (Either DbError (UserId, User))
lookupUser uid clearance = runThentosQuery clearance $ _lookupUser uid

_lookupUser :: UserId -> ThentosQuery (UserId, User)
_lookupUser uid = (uid,) <$$> do
    perhaps :: Maybe User <- Map.lookup uid . (^. dbUsers) <$> ask
    maybe (throwDBQ dcPublic NoSuchUser) (returnDBQ dcPublic) perhaps

-- FIXME: this is extremely inefficient, we should have a separate map from
    -- user names to users or user ids
lookupUserByName :: UserName -> ThentosClearance -> Query DB (Either DbError (UserId, User))
lookupUserByName name clearance = runThentosQuery clearance $ lookupUserId name
  where
    lookupUserId :: UserName -> ThentosQuery (UserId, User)
    lookupUserId name = do
        users <- Map.toList . (^. dbUsers) <$> ask
        let mUser = find (\(uid, user) -> (user ^. userName == name)) users
        maybe (throwDBQ dcPublic NoSuchUser) (returnDBQ dcPublic) mUser

-- | Write new user to DB.  Return the fresh user id.
addUser :: User -> ThentosClearance -> Update DB (Either DbError UserId)
addUser user clearance = runThentosUpdate clearance $ _addUser user

_addUser :: User -> ThentosUpdate UserId
_addUser user = do
    ThentosLabeled (ThentosLabel label) uid <- _freshUserID
    __writeUser uid user

-- | Write a list of new users to DB.  Return list of fresh user ids.
-- This is not the most vital part of the backend API, but it allows
-- for testing rollback in error cases.  It will also be a nice
-- example for intersecting authorizations.
addUsers :: [User] -> ThentosClearance -> Update DB (Either DbError [UserId])
addUsers users clearance = runThentosUpdate clearance $ _addUsers users

_addUsers :: [User] -> ThentosUpdate [UserId]
_addUsers users = mapM _addUser users >>= returnDBU dcPublic . map (\ (ThentosLabeled _ uid) -> uid)

-- | (db ^. dbUser) must only be modified using this function.
-- (FIXME: we should have a more generic approach to smart accessors
-- to DB that makes it clear invariants always hold.)
__writeUser :: UserId -> User -> ThentosUpdate UserId
__writeUser uid user = do
    liftThentosQuery $ checkDbInvs [dbInvUserEmailUnique uid user]
    modify $ dbUsers %~ Map.insert uid user
    returnDBU dcPublic uid

-- | Update existing user in DB.  Throw an error if user id does not
-- exist, or if email address in updated user is already in use by
-- another user.
updateUser :: UserId -> User -> ThentosClearance -> Update DB (Either DbError ())
updateUser uid user clearance = runThentosUpdate clearance $ _updateUser uid user

_updateUser :: UserId -> User -> ThentosUpdate ()
_updateUser uid user = do
    liftThentosQuery $ _lookupUser uid
    __writeUser uid user
    returnDBU dcPublic ()

-- | Delete user with given user id.  If user does not exist, throw an
-- error.
deleteUser :: UserId -> ThentosClearance -> Update DB (Either DbError ())
deleteUser uid clearance = runThentosUpdate clearance $ _deleteUser uid

_deleteUser :: UserId -> ThentosUpdate ()
_deleteUser uid = do
    liftThentosQuery $ _lookupUser uid
    modify $ dbUsers %~ Map.delete uid
    returnDBU dcPublic ()


-- ** services

allServiceIDs :: ThentosClearance -> Query DB (Either DbError [ServiceId])
allServiceIDs clearance = runThentosQuery clearance _allServiceIDs

_allServiceIDs :: ThentosQuery [ServiceId]
_allServiceIDs = thentosLabeledPublic . Map.keys . (^. dbServices) <$> ask

lookupService :: ServiceId -> ThentosClearance -> Query DB (Either DbError (ServiceId, Service))
lookupService sid clearance = runThentosQuery clearance $ _lookupService sid

_lookupService :: ServiceId -> ThentosQuery (ServiceId, Service)
_lookupService sid = (sid,) <$$> do
    perhaps :: Maybe Service <- Map.lookup sid . (^. dbServices) <$> ask
    maybe (throwDBQ dcPublic NoSuchService) (returnDBQ dcPublic) perhaps

-- | Write new service to DB.  Service key is generated automatically.
-- Return fresh service id.
addService :: ThentosClearance -> Update DB (Either DbError ServiceId)
addService clearance = runThentosUpdate clearance _addService

_addService :: ThentosUpdate ServiceId
_addService = do
    ThentosLabeled _ sid <- _freshServiceID
    ThentosLabeled _ service <- Service <$$> _freshServiceKey
    modify $ dbServices %~ Map.insert sid service
    returnDBU dcPublic sid

deleteService :: ServiceId -> ThentosClearance -> Update DB (Either DbError ())
deleteService sid clearance = runThentosUpdate clearance $ _deleteService sid

_deleteService :: ServiceId -> ThentosUpdate ()
_deleteService sid = do
    liftThentosQuery $ _lookupService sid
    modify $ dbServices %~ Map.delete sid
    returnDBU dcPublic ()

-- FIXME: we don't have any api (neither in DB nor here) to manage
-- user's group data.


-- ** sessions

-- | Start a new session for user with 'UserID' on service with
-- 'ServiceID'.  Start and end time have to be passed explicitly.
-- Throw error if user or service do not exist, or if user is already
-- logged in.  Otherwise, return session token.
startSession :: UserId -> ServiceId -> TimeStamp -> TimeStamp -> ThentosClearance -> Update DB (Either DbError SessionToken)
startSession uid sid start end clearance = runThentosUpdate clearance $ _startSession uid sid start end

_startSession :: UserId -> ServiceId -> TimeStamp -> TimeStamp -> ThentosUpdate SessionToken
_startSession uid sid start end = do
    ThentosLabeled _ tok       <- _freshSessionToken
    ThentosLabeled _ (_, user) <- liftThentosQuery $ _lookupUser uid
    ThentosLabeled _ _         <- liftThentosQuery $ _lookupService sid
    let session = Session uid sid start end

    when' (isJust . lookup sid $ user ^. userSessions) $
      throwDBU dcPublic SessionAlreadyExists

    modify $ dbSessions %~ Map.insert tok session
    modify $ dbUsers %~ Map.insert uid (userSessions %~ ((sid, tok):) $ user)
    returnDBU dcPublic tok

allSessionTokens :: ThentosClearance -> Query DB (Either DbError [SessionToken])
allSessionTokens clearance = runThentosQuery clearance _allSessionTokens

_allSessionTokens :: ThentosQuery [SessionToken]
_allSessionTokens = thentosLabeledPublic . Map.keys . (^. dbSessions) <$> ask

lookupSession :: SessionToken -> ThentosClearance -> Query DB (Either DbError (SessionToken, Session))
lookupSession tok clearance = runThentosQuery clearance $ _lookupSession tok

_lookupSession :: SessionToken -> ThentosQuery (SessionToken, Session)
_lookupSession tok = (tok,) <$$> do
    perhaps :: Maybe Session <- Map.lookup tok . (^. dbSessions) <$> ask
    maybe (throwDBQ dcPublic NoSuchSession) (returnDBQ dcPublic) perhaps

-- | End session.  Call can be caused by logout request from user
-- (before end of session life time), by session timeouts, or after
-- its natural life time (by application's own garbage collection).
-- If lookup of session owning user fails, throw an error.
endSession :: SessionToken -> ThentosClearance -> Update DB (Either DbError ())
endSession tok clearance = runThentosUpdate clearance $ _endSession tok

_endSession :: SessionToken -> ThentosUpdate ()
_endSession tok = do
    ThentosLabeled _ (_, Session uid sid _ _) <- liftThentosQuery $ _lookupSession tok
    ThentosLabeled _ (_, user)                <- liftThentosQuery $ _lookupUser uid
    modify $ dbSessions %~ Map.delete tok
    modify $ dbUsers %~ Map.insert uid (userSessions %~ filter (/= (sid, tok)) $ user)
    returnDBU dcPublic ()

-- | Is session token currently valid in the context of a given
-- service?
--
-- (we may want to drop the 'ServiceId' from the arguments, and
-- instead ensure via some yet-to-come authorization mechanism that
-- only the affected service can gets validity information on a
-- session token.)
isActiveSession :: ServiceId -> SessionToken -> ThentosClearance -> Query DB (Either DbError Bool)
isActiveSession sid tok clearance = runThentosQuery clearance $ _isActiveSession sid tok

_isActiveSession :: ServiceId -> SessionToken -> ThentosQuery Bool
_isActiveSession sid tok = do
    mSession :: Maybe Session <- Map.lookup tok . (^. dbSessions) <$> ask
    returnDBQ dcPublic $ maybe False ((sid ==) . (^. sessionService)) mSession


-- * event types

$(makeAcidic ''DB
    [ 'allUserIDs
    , 'lookupUser
    , 'lookupUserByName
    , 'addUser
    , 'addUsers
    , 'updateUser
    , 'deleteUser

    , 'allServiceIDs
    , 'lookupService
    , 'addService
    , 'deleteService

    , 'startSession
    , 'allSessionTokens
    , 'lookupSession
    , 'endSession
    , 'isActiveSession
    ])


-- * convenience

-- | Create a new thread that calls `createCheckpoint` synchronously,
-- then waits for @timeThreshold@ miliseconds, then repeats.  If
-- @sizeThreshold@ is `Just` a size, create checkpoint only if size of
-- segment of current change log since last checkpoint is larger than
-- that.
--
-- FIXME: check change log size.  (i think this is only possible
-- inside acid-state.)  https://github.com/acid-state/acid-state.
createCheckpointLoop :: AcidState st -> Int -> Maybe Int -> IO ThreadId
createCheckpointLoop acidState timeThreshold _ = forkIO iter
  where
    iter = do
      threadDelay $ timeThreshold * 1000

      -- when (isJust sizeThreshold) . assert False $
      --   print "createCheckpointLoop: sizeThreshold handling not implemented."

      createCheckpoint acidState
      iter
