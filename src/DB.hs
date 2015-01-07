{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE DeriveDataTypeable                       #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TemplateHaskell                          #-}
{-# LANGUAGE TypeFamilies                             #-}
{-# LANGUAGE TypeOperators                            #-}
{-# LANGUAGE ViewPatterns                             #-}

{-# OPTIONS  #-}

module DB
  ( AllUserIDs(..)
  , AllUsers(..)
  , LookupUser(..)
  , AddUser(..)
  , UpdateUser(..)
  , DeleteUser(..)

  , AllServiceIDs(..)
  , AllServices(..)
  , LookupService(..)
  , AddService(..)
  , DeleteService(..)

  , StartSession(..)
  , AllSessionTokens(..)
  , LookupSession(..)
  , EndSession(..)
  , IsActiveSession(..)

  , emptyDB
  , update_
  , createCheckpointLoop
  )
where

import Control.Concurrent (threadDelay, forkIO, ThreadId)
import Control.Lens ((^.), (.~), (%~))
import Control.Monad.Reader (ask)
import Control.Monad.State (modify, state)
import Control.Monad (when, void)
import Data.Acid (AcidState, createCheckpoint, EventState, liftQuery, makeAcidic, Query, update, Update, UpdateEvent)
import Data.Functor.Infix ((<$>))
import Data.Maybe (isJust)
import Data.String.Conversions (cs, ST, (<>))

import qualified Codec.Binary.Base32 as Base32
import qualified Crypto.Hash.SHA3 as Hash
import qualified Data.Map as Map

import Types


-- * event functions

emptyDB :: DB
emptyDB = DB Map.empty Map.empty Map.empty (UserId 0) ""

freshUserID :: Update DB UserId
freshUserID = state $ \ db -> f db (db ^. dbFreshUserId)
  where
    f db uid = if uid < maxBound
                 then (uid, dbFreshUserId .~ succ uid $ db)
                 else error "freshUserID: internal error: integer overflow!"

freshServiceID :: Update DB ServiceId
freshServiceID = ServiceId <$> freshNonce

freshServiceKey :: Update DB ServiceKey
freshServiceKey = ServiceKey <$> freshNonce

freshSessionToken :: Update DB SessionToken
freshSessionToken = SessionToken <$> freshNonce

-- | this makes the impression of a cryptographic function, but there
-- is no adversary model and no promise of security.  just yield
-- seemingly random service ids, and update randomness in `DB`.
freshNonce :: Update DB ST
freshNonce = state $ \ db ->
  let r   = db ^. dbRandomness
      r'  = Hash.hash 512 r
      db' = dbRandomness .~ r' $ db
      sid = cs . Base32.encode . Hash.hash 512 $ "_" <> r
  in (sid, db')


-- ** users

allUserIDs :: Query DB [UserId]
allUserIDs = Map.keys . (^. dbUsers) <$> ask

allUsers :: Query DB [User]
allUsers = Map.elems . (^. dbUsers) <$> ask

lookupUser :: UserId -> Query DB (Maybe User)
lookupUser uid = Map.lookup uid . (^. dbUsers) <$> ask

-- | Write new user to DB.  Return the fresh user id.
addUser :: User -> Update DB UserId
addUser user = do
  uid <- freshUserID
  modify $ dbUsers %~ Map.insert uid user
  return uid

-- | Update existing user in DB.  Throw an error if user id does not
-- exist.
updateUser :: UserId -> User -> Update DB ()
updateUser uid user =
  modify $ dbUsers %~ Map.alter (\ (Just _) -> Just user) uid  -- FIXME: error handling.

-- | Delete user with given user id.  If user does not exist, do nothing.
deleteUser :: UserId -> Update DB ()
deleteUser uid = modify $ dbUsers %~ Map.delete uid


-- ** services

allServiceIDs :: Query DB [ServiceId]
allServiceIDs = Map.keys . (^. dbServices) <$> ask

allServices :: Query DB [Service]
allServices = Map.elems . (^. dbServices) <$> ask

lookupService :: ServiceId -> Query DB (Maybe Service)
lookupService sid = Map.lookup sid . (^. dbServices) <$> ask

-- | Write new service to DB.  Service key is generated automatically.
-- Return fresh service id.
addService :: Update DB ServiceId
addService = do
  sid <- freshServiceID
  service <- Service <$> freshServiceKey
  modify $ dbServices %~ Map.insert sid service
  return sid

deleteService :: ServiceId -> Update DB ()
deleteService sid = modify $ dbServices %~ Map.delete sid


-- ** sessions

-- | Start a new session for user with 'UserID' on service with
-- 'ServiceID'.  Start and end time have to be passed explicitly.
-- Throw exception if user or service do not exist.  Return session
-- token.  If user is already logged in, throw an error.
--
-- FIXME: shouldn't users be able to log in on several services?
--
-- FIXME: how do you do errors / exceptions in acid-state?  at least
-- we should throw typed exceptions, not just strings, right?
startSession :: UserId -> ServiceId -> TimeStamp -> TimeStamp -> Update DB SessionToken
startSession uid sid start end = do
  tok <- freshSessionToken
  Just user <- liftQuery $ lookupUser uid  -- FIXME: error handling
  Just _ <- liftQuery $ lookupService sid  -- FIXME: error handling
  let session = Session uid sid start end

  when (isJust $ user ^. userSession) $
    error "startSession: user already logged in."  -- FIXME: error handling

  modify $ dbSessions %~ Map.insert tok session
  modify $ dbUsers %~ Map.insert uid (userSession .~ Just session $ user)
  return tok


allSessionTokens :: Query DB [SessionToken]
allSessionTokens = Map.keys . (^. dbSessions) <$> ask

lookupSession :: SessionToken -> Query DB (Maybe Session)
lookupSession tok = Map.lookup tok . (^. dbSessions) <$> ask


-- | End session.  Call can be caused by logout request from user
-- (before end of session life time), by session timeouts, or after
-- its natural life time (by application's own garbage collection).
-- If lookup of session owning user fails, throw an error.
--
-- FIXME: check if user has session set to @Nothing@, or to the wrong
-- session?
--
-- FIXME: what about exceptions in acid state?
endSession :: SessionToken -> Update DB ()
endSession tok = do
  Just (Session uid _ _ _) <- liftQuery $ lookupSession tok
  Just user <- liftQuery $ lookupUser uid  -- FIXME: error handling.
  modify $ dbSessions %~ Map.delete tok
  modify $ dbUsers %~ Map.insert uid (userSession .~ Nothing $ user)


-- | Is session token currently valid in the context of a given
-- service?
--
-- (we may want to drop the 'ServiceId' from the arguments, and
-- instead ensure via some yet-to-come authorization mechanism that
-- only the affected service can gets validity information on a
-- session token.)
isActiveSession :: ServiceId -> SessionToken -> Query DB Bool
isActiveSession sid tok = do
  mSession :: Maybe Session <- Map.lookup tok . (^. dbSessions) <$> ask
  case mSession of
    Nothing -> return False
    Just session -> return $ sid == session ^. sessionService


-- * event types

$(makeAcidic ''DB
    [ 'freshUserID
    , 'freshServiceID
    , 'freshSessionToken
    , 'freshNonce

    , 'allUserIDs
    , 'allUsers
    , 'lookupUser
    , 'addUser
    , 'updateUser
    , 'deleteUser

    , 'allServiceIDs
    , 'allServices
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

update_ :: UpdateEvent event => AcidState (EventState event) -> event -> IO ()
update_ st e = void $ update st e


-- | Create a new thread that calls `createCheckpoint` synchronously,
-- then waits for @timeThreshold@ miliseconds, then repeats.  If
-- @sizeThreshold@ is `Just` a changes log size, create checkpoint
-- only if actual change log size is larger.
--
-- FIXME: check change log size.  (i think this is only possible
-- inside acid-state.)
--
-- FIXME: make this a pull request for
-- https://github.com/acid-state/acid-state.
createCheckpointLoop :: AcidState st -> Int -> Maybe Int -> IO ThreadId
createCheckpointLoop acidState timeThreshold _ = forkIO iter
  where
    iter = do
      threadDelay $ timeThreshold * 1000

      -- when (isJust sizeThreshold) . assert False $
      --   print "createCheckpointLoop: sizeThreshold handling not implemented."

      createCheckpoint acidState
      iter
