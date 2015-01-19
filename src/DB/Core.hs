{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE DeriveDataTypeable                       #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TemplateHaskell                          #-}
{-# LANGUAGE TypeFamilies                             #-}
{-# LANGUAGE TypeOperators                            #-}
{-# LANGUAGE ViewPatterns                             #-}

{-# OPTIONS  #-}

module DB.Core
  ( AllUserIDs(..)
  , LookupUser(..)
  , AddUser(..)
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
import Control.Monad.State (modify, state, gets)
import Control.Monad (when)
import Data.Acid (AcidState, EventState, EventResult, UpdateEvent, Query, Update, createCheckpoint, liftQuery, makeAcidic)
import Data.Acid.Advanced (Method(..))
import Data.Functor.Infix ((<$>), (<$$>))
import Data.Maybe (isJust)
import Data.String.Conversions (cs, ST, (<>))
import LIO (canFlowTo)
import LIO.DCLabel (DCLabel, (%%), dcPublic)

import qualified Codec.Binary.Base32 as Base32
import qualified Crypto.Hash.SHA3 as Hash
import qualified Data.Map as Map

import Types
import DB.Error
import DB.Protect


-- * event functions

emptyDB :: DB
emptyDB = DB Map.empty Map.empty Map.empty Map.empty (UserId 0) ""

freshUserID :: ThentosClearance -> Update DB (Either DbError UserId)
freshUserID clearance = runThentosUpdate clearance _freshUserID

_freshUserID :: ThentosUpdate UserId
_freshUserID = do
    uid <- gets (^. dbFreshUserId)
    when' (uid == maxBound) $
        throwDB dcPublic UidOverflow
    modify (dbFreshUserId .~ succ uid)
    returnDB dcPublic uid

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

lookupUser :: UserId -> ThentosClearance -> Query DB (Either DbError User)
lookupUser uid clearance = runThentosQuery clearance $ _lookupUser uid

_lookupUser :: UserId -> ThentosQuery User
_lookupUser uid = do
    perhaps :: Maybe User <- Map.lookup uid . (^. dbUsers) <$> ask
    maybe (throwDBQ dcPublic NoSuchUser) (returnDBQ dcPublic) perhaps

-- | Write new user to DB.  Return the fresh user id.
addUser :: User -> ThentosClearance -> Update DB (Either DbError UserId)
addUser user clearance = runThentosUpdate clearance $ _addUser user

_addUser :: User -> ThentosUpdate UserId
_addUser user = do
  ThentosLabeled (ThentosLabel label) uid <- _freshUserID
  modify $ dbUsers %~ Map.insert uid user
  returnDB label uid

-- | Update existing user in DB.  Throw an error if user id does not
-- exist.
updateUser :: UserId -> User -> ThentosClearance -> Update DB (Either DbError ())
updateUser uid user clearance = runThentosUpdate clearance $ _updateUser uid user

_updateUser :: UserId -> User -> ThentosUpdate ()
_updateUser uid user = do
  modify $ dbUsers %~ Map.alter (\ (Just _) -> Just user) uid
  returnDB dcPublic ()

-- | Delete user with given user id.  If user does not exist, do nothing.
deleteUser :: UserId -> ThentosClearance -> Update DB (Either DbError ())
deleteUser uid clearance = runThentosUpdate clearance $ _deleteUser uid

_deleteUser :: UserId -> ThentosUpdate ()
_deleteUser uid = do
  modify $ dbUsers %~ Map.delete uid
  returnDB dcPublic ()


-- ** services

allServiceIDs :: ThentosClearance -> Query DB (Either DbError [ServiceId])
allServiceIDs clearance = runThentosQuery clearance _allServiceIDs

_allServiceIDs :: ThentosQuery [ServiceId]
_allServiceIDs = thentosLabeledPublic . Map.keys . (^. dbServices) <$> ask

lookupService :: ServiceId -> ThentosClearance -> Query DB (Either DbError Service)
lookupService sid clearance = runThentosQuery clearance $ _lookupService sid

_lookupService :: ServiceId -> ThentosQuery Service
_lookupService sid = do
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
    returnDB dcPublic sid

deleteService :: ServiceId -> ThentosClearance -> Update DB (Either DbError ())
deleteService sid clearance = runThentosUpdate clearance $ _deleteService sid

_deleteService :: ServiceId -> ThentosUpdate ()
_deleteService sid = do
  modify $ dbServices %~ Map.delete sid
  returnDB dcPublic ()

-- FIXME: we don't have any api (neither in DB nor here) to manage
-- user's group data.


-- ** sessions

-- | Start a new session for user with 'UserID' on service with
-- 'ServiceID'.  Start and end time have to be passed explicitly.
-- Throw exception if user or service do not exist.  Return session
-- token.  If user is already logged in, throw an error.
startSession :: UserId -> ServiceId -> TimeStamp -> TimeStamp -> ThentosClearance -> Update DB (Either DbError SessionToken)
startSession uid sid start end clearance = runThentosUpdate clearance $ _startSession uid sid start end

_startSession :: UserId -> ServiceId -> TimeStamp -> TimeStamp -> ThentosUpdate SessionToken
_startSession uid sid start end = do
  ThentosLabeled _ tok  <- _freshSessionToken
  ThentosLabeled _ user <- liftThentosQuery $ _lookupUser uid
  ThentosLabeled _ _    <- liftThentosQuery $ _lookupService sid
  let session = Session uid sid start end

  when' (isJust . lookup sid $ user ^. userSessions) $
    throwDB dcPublic SessionAlreadyExists

  modify $ dbSessions %~ Map.insert tok session
  modify $ dbUsers %~ Map.insert uid (userSessions %~ ((sid, tok):) $ user)
  returnDB dcPublic tok


allSessionTokens :: ThentosClearance -> Query DB (Either DbError [SessionToken])
allSessionTokens clearance = runThentosQuery clearance _allSessionTokens

_allSessionTokens :: ThentosQuery [SessionToken]
_allSessionTokens = thentosLabeledPublic . Map.keys . (^. dbSessions) <$> ask


lookupSession :: SessionToken -> ThentosClearance -> Query DB (Either DbError Session)
lookupSession tok clearance = runThentosQuery clearance $ _lookupSession tok

_lookupSession :: SessionToken -> ThentosQuery Session
_lookupSession tok = do
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
  ThentosLabeled _ (Session uid sid _ _) <- liftThentosQuery $ _lookupSession tok
  ThentosLabeled _ user                  <- liftThentosQuery $ _lookupUser uid
  modify $ dbSessions %~ Map.delete tok
  modify $ dbUsers %~ Map.insert uid (userSessions %~ filter (/= (sid, tok)) $ user)
  returnDB dcPublic ()


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
_isActiveSession sid tok = thentosLabeledPublic <$> do
  mSession :: Maybe Session <- Map.lookup tok . (^. dbSessions) <$> ask
  case mSession of
    Nothing -> return False
    Just session -> return $ sid == session ^. sessionService


-- * event types

$(makeAcidic ''DB
    [ 'allUserIDs
    , 'lookupUser
    , 'addUser
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
