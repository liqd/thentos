{-# LANGUAGE DeriveDataTypeable                       #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TemplateHaskell                          #-}
{-# LANGUAGE TypeFamilies                             #-}
{-# LANGUAGE ViewPatterns                             #-}
{-# LANGUAGE OverloadedStrings                        #-}

{-# OPTIONS -fwarn-unused-imports -fwarn-incomplete-patterns -fwarn-typed-holes -fdefer-type-errors #-}

module DB
where

import Control.Concurrent  -- FIXME: no unqualified imports.  ever.
import Control.Exception (assert)
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.Functor.Infix
import Data.Maybe
import Data.Monoid
import Data.String.Conversions
import Data.Thyme

import qualified Codec.Binary.Base32 as Base32
import qualified Crypto.Hash.SHA3 as Hash
import qualified Data.Map as Map

import Types


-- * event functions

emptyDB :: DB
emptyDB = DB Map.empty Map.empty Map.empty 0 ""

freshUserID :: Update DB UserID
freshUserID = state $ \ db -> f db (db ^. dbFreshUserID)
  where
    f db uid = if uid < maxBound
                 then (uid, dbFreshUserID %~ const (uid + 1) $ db)
                 else error "freshUserID: internal error: integer overflow!"

freshServiceID :: Update DB ServiceID
freshServiceID = freshNonce

freshSessionToken :: Update DB SessionToken
freshSessionToken = freshNonce

-- | this makes the impression of a cryptographic function, but there
-- is no adversary model and no promise of security.  just yield
-- seemingly random service ids, and update randomness in `DB`.
freshNonce :: Update DB ST
freshNonce = state $ \ db ->
  let r   = db ^. dbRandomness
      r'  = Hash.hash 512 r
      db' = dbRandomness %~ const r' $ db
      sid = cs . Base32.encode . Hash.hash 512 $ "_" <> r
  in (sid, db')


-- ** users

allUserIDs :: Query DB [UserID]
allUserIDs = Map.keys . (^. dbUsers) <$> ask

allUsers :: Query DB [User]
allUsers = Map.elems . (^. dbUsers) <$> ask

lookupUser :: UserID -> Query DB (Maybe User)
lookupUser uid = Map.lookup uid . (^. dbUsers) <$> ask

-- | Write new user to DB.  Return the fresh user id.
addUser :: User -> Update DB UserID
addUser user = do
  uid <- freshUserID
  modify $ dbUsers %~ Map.insert uid user
  return uid

-- | Update existing user in DB.  Throw an error if user id does not
-- exist.
updateUser :: UserID -> User -> Update DB ()
updateUser uid user = do
  modify $ dbUsers %~ Map.alter (\ (Just _) -> Just user) uid  -- FIXME: error handling.

-- | Delete user with given user id.  If user does not exist, do nothing.
deleteUser :: UserID -> Update DB ()
deleteUser uid = modify $ dbUsers %~ Map.delete uid


-- ** services

allServiceIDs :: Query DB [ServiceID]
allServiceIDs = Map.keys . (^. dbServices) <$> ask

allServices :: Query DB [Service]
allServices = Map.elems . (^. dbServices) <$> ask

lookupService :: ServiceID -> Query DB (Maybe Service)
lookupService sid = Map.lookup sid . (^. dbServices) <$> ask

-- | Write new service to DB.  Return fresh service id.
addService :: Service -> Update DB ServiceID
addService service = do
  sid <- freshServiceID
  modify $ dbServices %~ Map.insert sid service
  return sid

-- | Update existing service in DB.  Throw an error if service does
-- not exist.
updateService :: ServiceID -> Service -> Update DB ()
updateService sid service = modify $ dbServices %~ Map.alter (\ (Just _) -> Just service) sid  -- FIXME: error handling.

deleteService :: ServiceID -> Update DB ()
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
startSession :: UserID -> ServiceID -> UTCTime -> UTCTime -> Update DB SessionToken
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
  Just (session@(Session uid _ start end)) <- liftQuery $ lookupSession tok
  Just user <- liftQuery $ lookupUser uid  -- FIXME: error handling.
  modify $ dbSessions %~ Map.delete tok
  modify $ dbUsers %~ Map.insert uid (userSession .~ Nothing $ user)


-- | Is session token currently valid?
isActiveSession :: SessionToken -> Query DB Bool
isActiveSession tok = Map.member tok . (^. dbSessions) <$> ask


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

    , 'allServices
    , 'lookupService
    , 'addService
    , 'updateService
    , 'deleteService

    , 'startSession
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
createCheckpointLoop acidState timeThreshold sizeThreshold = forkIO iter
  where
    iter = do
      threadDelay $ timeThreshold * 1000

      -- when (isJust sizeThreshold) . assert False $
      --   print "createCheckpointLoop: sizeThreshold handling not implemented."

      createCheckpoint acidState
      iter


createCheckpointInterruptHandler :: AcidState st -> IO ()
createCheckpointInterruptHandler = assert False $ error "createCheckpointIntHandler"


-- FIXME: what to do about authorisations?  lio (see paper in research
-- git repo)?  yesod?  snap?  does servant have an answer?
