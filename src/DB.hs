{-# LANGUAGE DeriveDataTypeable                       #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TemplateHaskell                          #-}
{-# LANGUAGE TypeFamilies                             #-}
{-# LANGUAGE ViewPatterns                             #-}
{-# LANGUAGE OverloadedStrings                        #-}

{-# OPTIONS -fwarn-unused-imports -fwarn-incomplete-patterns -fwarn-typed-holes -fdefer-type-errors #-}

module DB
where

import Control.Applicative hiding ((<$>))
import Control.Concurrent
import Control.Exception (assert)
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.Functor.Infix
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

allUsers :: Query DB [User]
allUsers = Map.elems . (^. dbUsers) <$> ask

lookupUser :: UserID -> Query DB (Maybe User)
lookupUser uid = Map.lookup uid . (^. dbUsers) <$> ask

-- | write user to DB.  if user id is already taken, overwrite
-- existing user.  if user id is Nothing, create a fresh one.
-- return the final user id.
insertUser :: User -> Update DB UserID
insertUser user = do
    let establishUserID :: Update DB UserID
        establishUserID = maybe freshUserID pure $ user ^. userID

        giveUserID :: UserID -> User -> User
        giveUserID uid = userID %~ const (Just uid)

    uid <- establishUserID
    modify $ dbUsers %~ Map.insert uid (giveUserID uid user)
    return uid

-- | delete user with given user id.  if user does not exist, do nothing.
deleteUser :: UserID -> Update DB ()
deleteUser uid = modify $ dbUsers %~ Map.delete uid


-- ** services

allServices :: Query DB [Service]
allServices = Map.elems . (^. dbServices) <$> ask

lookupService :: ServiceID -> Query DB (Maybe Service)
lookupService sid = Map.lookup sid . (^. dbServices) <$> ask

-- | write service to DB.  if service id is already taken, overwrite
-- existing service.  if service id is Nothing, create a fresh one.
-- return the final service id.
insertService :: Service -> Update DB ServiceID
insertService service = do
    let establishServiceID :: Update DB ServiceID
        establishServiceID = maybe freshServiceID pure $ service ^. serviceID

        giveServiceID :: ServiceID -> Service -> Service
        giveServiceID sid = serviceID %~ const (Just sid)

    sid <- establishServiceID
    modify $ dbServices %~ Map.insert sid (giveServiceID sid service)
    return sid

deleteService :: ServiceID -> Update DB ()
deleteService sid = modify $ dbServices %~ Map.delete sid


-- ** sessions

-- | Start a new session for user with 'UserID'.  Session token, start
-- and end time have to be passed explicitly.  Throw exception if user
-- id is not allocated.  If user is already logged in, overwrite old
-- session.
--
-- FIXME: this is a bug: overwrite session1 in user1 with session2,
-- then end session1, then session2 in user1 will be dropped (even
-- though it is still valid).
--
-- FIXME: what about exceptions in acid state?
startSession :: UserID -> UTCTime -> UTCTime -> Update DB SessionToken
startSession uid start end = do
  session <- (\ tok -> Session tok uid start end) <$> freshSessionToken
  Just user <- liftQuery $ lookupUser uid
  modify $ dbSessions %~ Map.insert (session ^. sessionToken) session
  modify $ dbUsers %~ Map.insert uid (userSession .~ Just session $ user)
  return $ session ^. sessionToken


lookupSession :: SessionToken -> Query DB (Maybe Session)
lookupSession tok = Map.lookup tok . (^. dbSessions) <$> ask


-- | End session.  Call can be caused by logout request from user
-- (before end of session life time), by session timeouts, or after
-- its natural life time by garbage collection.  If lookup of session
-- owning user fails, throw an exception.
--
-- FIXME: what if user has session set to @Nothing@, or to the wrong
-- session?
--
-- FIXME: what about exceptions in acid state?
endSession :: SessionToken -> Update DB ()
endSession tok = do
  Just (session@(Session tok uid start end)) <- liftQuery $ lookupSession tok
  Just user <- liftQuery $ lookupUser uid
  modify $ dbSessions %~ Map.delete (session ^. sessionToken)
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

    , 'allUsers
    , 'lookupUser
    , 'insertUser
    , 'deleteUser

    , 'allServices
    , 'lookupService
    , 'insertService
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
