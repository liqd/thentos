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
  , queryLIO
  , updateLIO
  , updateLIO_
  , createCheckpointLoop
  )
where

import Control.Concurrent (threadDelay, forkIO, ThreadId)
import Control.Lens ((^.), (.~), (%~))
import Control.Monad.Reader (ask)
import Control.Monad.State (modify, state)
import Control.Monad (when, void)
import Data.Acid (AcidState, Query, Update, UpdateEvent, QueryEvent, EventState, EventResult,
                  createCheckpoint, liftQuery, makeAcidic)
import Data.Acid.Advanced (update', query')
import Data.Functor.Infix ((<$>))
import Data.IORef (readIORef)
import Data.Maybe (isJust)
import Data.SafeCopy (SafeCopy, contain, putCopy, getCopy, safePut, safeGet)
import Data.String.Conversions (cs, ST, (<>))
import Data.Typeable (Typeable)
import LIO (canFlowTo, lioClearance)
import LIO.DCLabel (DCLabel, (%%))
import LIO.TCB (LIO(LIOTCB))
import Safe (readMay)

import qualified Codec.Binary.Base32 as Base32
import qualified Crypto.Hash.SHA3 as Hash
import qualified Data.Map as Map

import Types


-- * lio

-- | Wrapper for LabeledTCB to avoid orphan instances.  (Also, freeze
-- 'DCLabel' as label type.)
data Labeled t = LabeledTCB DCLabel t
  deriving (Eq, Ord, Show, Read, Typeable)

instance (SafeCopy t, Show t, Read t) => SafeCopy (Labeled t)
  where
    putCopy = contain . safePut . show
    getCopy = contain $ safeGet >>= \ raw ->
      maybe (fail $ "instance SafeCopy Labeled: no parse" ++ show raw) return . readMay $ raw

-- | Run a query action that returns a labeled result.  Check that the
-- clearance is sufficient for the label, and return the unlabelled
-- result.
queryLIO :: (QueryEvent event, EventResult event ~ Labeled a) => AcidState (EventState event) -> event -> LIO DCLabel a
queryLIO st ev = LIOTCB $ \ stateRef -> do
  clearance :: DCLabel <- lioClearance <$> readIORef stateRef
  LabeledTCB (context :: DCLabel) result <- query' st ev
  if context `canFlowTo` clearance  -- FIXME: or is it supposed flow the other way?
    then return result
    else fail "authorization denied"  -- FIXME: throw error type here.

    -- NOTE: auth errors must always be caught first, before any other
    -- errors, or else non-authorization errors may leak information
    -- to unauthorized parties.

-- | Like 'queryLIO'.  If an action is not authorized, it will be
-- undone after the fact.  Not ideal, but lazyness may save us here.
-- An alternative implementation would create a type class, make all
-- 'UpdateEvent's instances, and ask the instances for the label.
-- That could be done before the operation, but then we would have no
-- dynamic context to compute the label with.
updateLIO :: (UpdateEvent event, EventResult event ~ Labeled a) => AcidState (EventState event) -> event -> LIO DCLabel a
updateLIO st ev = LIOTCB $ \ stateRef -> do
  clearance :: DCLabel <- lioClearance <$> readIORef stateRef
  LabeledTCB (context :: DCLabel) result <- update' st ev
  if context `canFlowTo` clearance
    then return result
    else fail "authorization denied"

-- | Call 'updateLIO' and discard the result.
updateLIO_ :: (UpdateEvent event, EventResult event ~ Labeled a) => AcidState (EventState event) -> event -> LIO DCLabel ()
updateLIO_ st = void . updateLIO st


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

allUserIDs :: Query DB (Labeled [UserId])
allUserIDs = LabeledTCB (True %% True) . Map.keys . (^. dbUsers) <$> ask

allUsers :: Query DB (Labeled [User])
allUsers = LabeledTCB (True %% True) . Map.elems . (^. dbUsers) <$> ask

lookupUser :: UserId -> Query DB (Labeled (Maybe User))
lookupUser uid = LabeledTCB (True %% True) . Map.lookup uid . (^. dbUsers) <$> ask

-- | Write new user to DB.  Return the fresh user id.
addUser :: User -> Update DB (Labeled UserId)
addUser user = do
  uid <- freshUserID
  modify $ dbUsers %~ Map.insert uid user
  return $ LabeledTCB (True %% True) uid

-- | Update existing user in DB.  Throw an error if user id does not
-- exist.
updateUser :: UserId -> User -> Update DB (Labeled ())
updateUser uid user = do
  modify $ dbUsers %~ Map.alter (\ (Just _) -> Just user) uid  -- FIXME: error handling.
  return $ LabeledTCB (True %% True) ()

-- | Delete user with given user id.  If user does not exist, do nothing.
deleteUser :: UserId -> Update DB (Labeled ())
deleteUser uid = do
  modify $ dbUsers %~ Map.delete uid
  return $ LabeledTCB (True %% True) ()


-- ** services

allServiceIDs :: Query DB (Labeled [ServiceId])
allServiceIDs = LabeledTCB (True %% True) . Map.keys . (^. dbServices) <$> ask

allServices :: Query DB (Labeled [Service])
allServices = LabeledTCB (True %% True) . Map.elems . (^. dbServices) <$> ask

lookupService :: ServiceId -> Query DB (Labeled (Maybe Service))
lookupService sid = LabeledTCB (True %% True) . Map.lookup sid . (^. dbServices) <$> ask

-- | Write new service to DB.  Service key is generated automatically.
-- Return fresh service id.
addService :: Update DB (Labeled ServiceId)
addService = do
  sid <- freshServiceID
  service <- Service <$> freshServiceKey
  modify $ dbServices %~ Map.insert sid service
  return $ LabeledTCB (True %% True) sid

deleteService :: ServiceId -> Update DB (Labeled ())
deleteService sid = do
  modify $ dbServices %~ Map.delete sid
  return $ LabeledTCB (True %% True) ()


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
startSession :: UserId -> ServiceId -> TimeStamp -> TimeStamp -> Update DB (Labeled SessionToken)
startSession uid sid start end = do
  tok <- freshSessionToken
  LabeledTCB _ (Just user) <- liftQuery $ lookupUser uid  -- FIXME: error handling
  LabeledTCB _ (Just _) <- liftQuery $ lookupService sid  -- FIXME: error handling
  let session = Session uid sid start end

  when (isJust $ user ^. userSession) $
    error "startSession: user already logged in."  -- FIXME: error handling

  modify $ dbSessions %~ Map.insert tok session
  modify $ dbUsers %~ Map.insert uid (userSession .~ Just session $ user)
  return $ LabeledTCB (True %% True) tok


allSessionTokens :: Query DB (Labeled [SessionToken])
allSessionTokens = LabeledTCB (True %% True) . Map.keys . (^. dbSessions) <$> ask

lookupSession :: SessionToken -> Query DB (Labeled (Maybe Session))
lookupSession tok = LabeledTCB (True %% True) . Map.lookup tok . (^. dbSessions) <$> ask


-- | End session.  Call can be caused by logout request from user
-- (before end of session life time), by session timeouts, or after
-- its natural life time (by application's own garbage collection).
-- If lookup of session owning user fails, throw an error.
--
-- FIXME: check if user has session set to @Nothing@, or to the wrong
-- session?
--
-- FIXME: what about exceptions in acid state?
endSession :: SessionToken -> Update DB (Labeled ())
endSession tok = do
  LabeledTCB _ (Just (Session uid _ _ _)) <- liftQuery $ lookupSession tok
  LabeledTCB _ (Just user) <- liftQuery $ lookupUser uid  -- FIXME: error handling.
  modify $ dbSessions %~ Map.delete tok
  modify $ dbUsers %~ Map.insert uid (userSession .~ Nothing $ user)
  return $ LabeledTCB (True %% True) ()


-- | Is session token currently valid in the context of a given
-- service?
--
-- (we may want to drop the 'ServiceId' from the arguments, and
-- instead ensure via some yet-to-come authorization mechanism that
-- only the affected service can gets validity information on a
-- session token.)
isActiveSession :: ServiceId -> SessionToken -> Query DB (Labeled Bool)
isActiveSession sid tok = LabeledTCB (True %% True) <$> do
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
