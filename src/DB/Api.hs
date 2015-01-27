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

module DB.Api
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

  , SnapShot(..)

  , pure_lookupUserByName

  , emptyDB
  , createCheckpointLoop
  )
where

import Control.Lens ((^.), (.~), (%~))
import Control.Monad (when, void)
import Control.Monad.Reader (ask)
import Control.Monad.State (modify, state, gets)
import Data.Acid (Query, Update, makeAcidic)
import Data.Functor.Infix ((<$>), (<$$>))
import Data.List (nub, find)
import Data.Maybe (isJust)
import Data.String.Conversions (cs, ST, (<>))
import LIO.DCLabel (dcPublic)

import qualified Codec.Binary.Base32 as Base32
import qualified Crypto.Hash.SHA3 as Hash
import qualified Data.Map as Map

import Types
import DB.Core


-- * DB invariants

checkDbInvs :: [DB -> Either DbError ()] -> ThentosQuery ()
checkDbInvs invs = do
    let f []           = returnDBQ dcPublic ()
        f (Left e:_)   = throwDBQ dcPublic e
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
emptyDB = DB Map.empty Map.empty Map.empty Map.empty (UserId 0) ""


-- ** smart accessors

freshUserID :: ThentosUpdate UserId
freshUserID = do
    uid <- gets (^. dbFreshUserId)
    when (uid == maxBound) . void $
        throwDBU dcPublic UidOverflow
    modify (dbFreshUserId .~ succ uid)
    returnDBU dcPublic uid

freshServiceID :: ThentosUpdate ServiceId
freshServiceID = ServiceId <$$> freshNonce

freshServiceKey :: ThentosUpdate ServiceKey
freshServiceKey = ServiceKey <$$> freshNonce

freshSessionToken :: ThentosUpdate SessionToken
freshSessionToken = SessionToken <$$> freshNonce

-- | this makes the impression of a cryptographic function, but there
-- is no adversary model and no promise of security.  just yield
-- seemingly random service ids, and update randomness in `DB`.
freshNonce :: ThentosUpdate ST
freshNonce = state $ \ db ->
  let r   = db ^. dbRandomness
      r'  = Hash.hash 512 r
      db' = dbRandomness .~ r' $ db
      sid = cs . Base32.encode . Hash.hash 512 $ "_" <> r
  in (thentosLabeledPublic sid, db')

-- | (db ^. dbUser) must only be modified using this function.
writeUser :: UserId -> User -> ThentosUpdate UserId
writeUser uid user = do
    _ <- liftThentosQuery $ checkDbInvs [dbInvUserEmailUnique uid user]
    modify $ dbUsers %~ Map.insert uid user
    returnDBU dcPublic uid


-- ** users

trans_allUserIDs :: ThentosQuery [UserId]
trans_allUserIDs = thentosLabeledPublic . Map.keys . (^. dbUsers) <$> ask

trans_lookupUser :: UserId -> ThentosQuery (UserId, User)
trans_lookupUser uid = (uid,) <$$> do
    perhaps :: Maybe User <- Map.lookup uid . (^. dbUsers) <$> ask
    maybe (throwDBQ dcPublic NoSuchUser) (returnDBQ dcPublic) perhaps

-- FIXME: this is extremely inefficient, we should have a separate map from
    -- user names to users or user ids
trans_lookupUserByName :: UserName -> ThentosQuery (UserId, User)
trans_lookupUserByName name = do
    mUser <- (`pure_lookupUserByName` name) <$> ask
    maybe (throwDBQ dcPublic NoSuchUser) (returnDBQ dcPublic) mUser

pure_lookupUserByName :: DB -> UserName -> Maybe (UserId, User)
pure_lookupUserByName db name =
    find (\ (_, user) -> (user ^. userName == name)) . Map.toList . (^. dbUsers) $ db

-- | Write new user to DB.  Return the fresh user id.
trans_addUser :: User -> ThentosUpdate UserId
trans_addUser user = do
    ThentosLabeled _ uid <- freshUserID
    writeUser uid user

-- | Write a list of new users to DB.  Return list of fresh user ids.
-- This is not the most vital part of the backend API, but it allows
-- for testing rollback in error cases.  It will also be a nice
-- example for intersecting authorizations.
trans_addUsers :: [User] -> ThentosUpdate [UserId]
trans_addUsers users = mapM trans_addUser users >>= returnDBU dcPublic . map (\ (ThentosLabeled _ uid) -> uid)

-- | Update existing user in DB.  Throw an error if user id does not
-- exist, or if email address in updated user is already in use by
-- another user.
trans_updateUser :: UserId -> User -> ThentosUpdate ()
trans_updateUser uid user = do
    _ <- liftThentosQuery $ trans_lookupUser uid
    _ <- writeUser uid user
    returnDBU dcPublic ()

-- | Delete user with given user id.  If user does not exist, throw an
-- error.
trans_deleteUser :: UserId -> ThentosUpdate ()
trans_deleteUser uid = do
    _ <- liftThentosQuery $ trans_lookupUser uid
    modify $ dbUsers %~ Map.delete uid
    returnDBU dcPublic ()


-- ** services

trans_allServiceIDs :: ThentosQuery [ServiceId]
trans_allServiceIDs = thentosLabeledPublic . Map.keys . (^. dbServices) <$> ask

trans_lookupService :: ServiceId -> ThentosQuery (ServiceId, Service)
trans_lookupService sid = (sid,) <$$> do
    perhaps :: Maybe Service <- Map.lookup sid . (^. dbServices) <$> ask
    maybe (throwDBQ dcPublic NoSuchService) (returnDBQ dcPublic) perhaps

-- | Write new service to DB.  Service key is generated automatically.
-- Return fresh service id.
trans_addService :: ThentosUpdate ServiceId
trans_addService = do
    ThentosLabeled _ sid <- freshServiceID
    ThentosLabeled _ service <- Service <$$> freshServiceKey
    modify $ dbServices %~ Map.insert sid service
    returnDBU dcPublic sid

trans_deleteService :: ServiceId -> ThentosUpdate ()
trans_deleteService sid = do
    _ <- liftThentosQuery $ trans_lookupService sid
    modify $ dbServices %~ Map.delete sid
    returnDBU dcPublic ()

-- FIXME: we don't have any api (neither in DB nor here) to manage
-- user's group data.


-- ** sessions

-- | Start a new session for user with 'UserID' on service with
-- 'ServiceID'.  Start and end time have to be passed explicitly.
-- Throw error if user or service do not exist, or if user is already
-- logged in.  Otherwise, return session token.
trans_startSession :: UserId -> ServiceId -> TimeStamp -> TimeStamp -> ThentosUpdate SessionToken
trans_startSession uid sid start end = do
    ThentosLabeled _ tok       <- freshSessionToken
    ThentosLabeled _ (_, user) <- liftThentosQuery $ trans_lookupUser uid
    ThentosLabeled _ _         <- liftThentosQuery $ trans_lookupService sid
    let session = Session uid sid start end

    when (isJust . lookup sid $ user ^. userSessions) . void $
      throwDBU dcPublic SessionAlreadyExists

    modify $ dbSessions %~ Map.insert tok session
    modify $ dbUsers %~ Map.insert uid (userSessions %~ ((sid, tok):) $ user)
    returnDBU dcPublic tok

trans_allSessionTokens :: ThentosQuery [SessionToken]
trans_allSessionTokens = thentosLabeledPublic . Map.keys . (^. dbSessions) <$> ask

trans_lookupSession :: SessionToken -> ThentosQuery (SessionToken, Session)
trans_lookupSession tok = (tok,) <$$> do
    perhaps :: Maybe Session <- Map.lookup tok . (^. dbSessions) <$> ask
    maybe (throwDBQ dcPublic NoSuchSession) (returnDBQ dcPublic) perhaps

-- | End session.  Call can be caused by logout request from user
-- (before end of session life time), by session timeouts, or after
-- its natural life time (by application's own garbage collection).
-- If lookup of session owning user fails, throw an error.
trans_endSession :: SessionToken -> ThentosUpdate ()
trans_endSession tok = do
    ThentosLabeled _ (_, Session uid sid _ _) <- liftThentosQuery $ trans_lookupSession tok
    ThentosLabeled _ (_, user)                <- liftThentosQuery $ trans_lookupUser uid
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
trans_isActiveSession :: ServiceId -> SessionToken -> ThentosQuery Bool
trans_isActiveSession sid tok = do
    mSession :: Maybe Session <- Map.lookup tok . (^. dbSessions) <$> ask
    returnDBQ dcPublic $ maybe False ((sid ==) . (^. sessionService)) mSession


-- ** misc

trans_snapShot :: ThentosQuery DB
trans_snapShot = ask >>= returnDBQ dcPublic


-- * event types

-- FIXME: this section should be entirely constructed by TemplateHaskell

allUserIDs :: ThentosClearance -> Query DB (Either DbError [UserId])
allUserIDs clearance = runThentosQuery clearance trans_allUserIDs

lookupUser :: UserId -> ThentosClearance -> Query DB (Either DbError (UserId, User))
lookupUser uid clearance = runThentosQuery clearance $ trans_lookupUser uid

lookupUserByName :: UserName -> ThentosClearance -> Query DB (Either DbError (UserId, User))
lookupUserByName name clearance = runThentosQuery clearance $ trans_lookupUserByName name

addUser :: User -> ThentosClearance -> Update DB (Either DbError UserId)
addUser user clearance = runThentosUpdate clearance $ trans_addUser user

addUsers :: [User] -> ThentosClearance -> Update DB (Either DbError [UserId])
addUsers users clearance = runThentosUpdate clearance $ trans_addUsers users

updateUser :: UserId -> User -> ThentosClearance -> Update DB (Either DbError ())
updateUser uid user clearance = runThentosUpdate clearance $ trans_updateUser uid user

deleteUser :: UserId -> ThentosClearance -> Update DB (Either DbError ())
deleteUser uid clearance = runThentosUpdate clearance $ trans_deleteUser uid

allServiceIDs :: ThentosClearance -> Query DB (Either DbError [ServiceId])
allServiceIDs clearance = runThentosQuery clearance trans_allServiceIDs

lookupService :: ServiceId -> ThentosClearance -> Query DB (Either DbError (ServiceId, Service))
lookupService sid clearance = runThentosQuery clearance $ trans_lookupService sid

addService :: ThentosClearance -> Update DB (Either DbError ServiceId)
addService clearance = runThentosUpdate clearance trans_addService

deleteService :: ServiceId -> ThentosClearance -> Update DB (Either DbError ())
deleteService sid clearance = runThentosUpdate clearance $ trans_deleteService sid

startSession :: UserId -> ServiceId -> TimeStamp -> TimeStamp -> ThentosClearance -> Update DB (Either DbError SessionToken)
startSession uid sid start end clearance = runThentosUpdate clearance $ trans_startSession uid sid start end

allSessionTokens :: ThentosClearance -> Query DB (Either DbError [SessionToken])
allSessionTokens clearance = runThentosQuery clearance trans_allSessionTokens

lookupSession :: SessionToken -> ThentosClearance -> Query DB (Either DbError (SessionToken, Session))
lookupSession tok clearance = runThentosQuery clearance $ trans_lookupSession tok

endSession :: SessionToken -> ThentosClearance -> Update DB (Either DbError ())
endSession tok clearance = runThentosUpdate clearance $ trans_endSession tok

isActiveSession :: ServiceId -> SessionToken -> ThentosClearance -> Query DB (Either DbError Bool)
isActiveSession sid tok clearance = runThentosQuery clearance $ trans_isActiveSession sid tok

snapShot :: ThentosClearance -> Query DB (Either DbError DB)
snapShot clearance = runThentosQuery clearance trans_snapShot


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

    , 'snapShot
    ])
