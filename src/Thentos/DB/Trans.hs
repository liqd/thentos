{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE DeriveDataTypeable                       #-}
{-# LANGUAGE MultiWayIf                               #-}
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

-- | This module provides a basic collection of acid-state
-- transactions on the state type 'DB'.
--
-- For each transaction, we export the acid-state constructor
-- @FooBloo@ to be passed to 'query' or 'update' and a function
-- 'trans_fooBloo' to be used as building blocks for writing new, more
-- complex transactions.
--
-- Lookup functions usually return the lookup key together with the
-- value.  The key can be discarded easy enough using pattern matching,
-- but in some cases it is useful to have it.
module Thentos.DB.Trans
  ( AllUserIds(..), trans_allUserIds
  , LookupUser(..), trans_lookupUser
  , LookupUserByName(..), trans_lookupUserByName
  , LookupUserByEmail(..), trans_lookupUserByEmail
  , AddUser(..), trans_addUser
  , AddUnconfirmedUser(..), trans_addUnconfirmedUser
  , FinishUserRegistration(..), trans_finishUserRegistration
  , AddUsers(..), trans_addUsers
  , UpdateUserField(..), trans_updateUserField, UpdateUserFieldOp(..)
  , UpdateUserFields(..)
  , DeleteUser(..), trans_deleteUser
  , AddUserEmailChangeRequest(..), trans_addUserEmailChangeRequest
  , ConfirmUserEmailChange(..), trans_confirmUserEmailChange
  , AddPasswordResetToken(..), trans_addPasswordResetToken
  , ResetPassword(..), trans_resetPassword

  , AllServiceIds(..), trans_allServiceIds
  , LookupService(..), trans_lookupService
  , AddService(..), trans_addService
  , DeleteService(..), trans_deleteService

  , AllSessionTokens(..), trans_allSessionTokens
  , LookupSessionQ(..), trans_lookupSessionQ
  , LookupSession(..), trans_lookupSession
  , StartSession(..), trans_startSession
  , EndSession(..), trans_endSession
  , IsActiveSession(..), trans_isActiveSession
  , IsActiveSessionAndBump(..), trans_isActiveSessionAndBump
  , IsLoggedIntoService(..), trans_isLoggedIntoService
  , IsRegisteredWithService(..), trans_isRegisteredWithService
  , AddServiceLogin(..), trans_addServiceLogin
  , DropServiceLogin(..), trans_dropServiceLogin
  , GetSessionServiceNames(..), trans_getSessionServiceNames
  , GarbageCollectSessions(..), trans_garbageCollectSessions

  --, GetServiceSessionMetaData(..), trans_getServiceSessionMetaData

  , AssignRole(..), trans_assignRole
  , UnassignRole(..), trans_unassignRole
  , LookupAgentRoles(..), trans_lookupAgentRoles

  , SnapShot(..), trans_snapShot

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
import Control.Monad (forM_)
import Control.Monad.Reader (ask)
import Control.Monad.State (modify, gets, get)
import Data.Acid (Query, Update, makeAcidic)
import Data.AffineSpace ((.+^))
import Data.EitherR (catchT)
import Data.Functor.Infix ((<$>))
import Data.List (find, (\\), foldl')
import Data.Maybe (fromMaybe, isJust)
import Data.SafeCopy (deriveSafeCopy, base)
import LIO.DCLabel ((\/), (/\))
import LIO.Label (lub)
import Safe (fromJustNote)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Thentos.DB.Core
import Thentos.Types


-- * event functions

emptyDB :: DB
emptyDB = DB Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty (UserId 0)


-- ** smart accessors

freshUserId :: ThentosUpdate' e UserId
freshUserId = do
    uid <- gets (^. dbFreshUserId)
    modify (dbFreshUserId .~ succ uid)
    return uid


-- ** users

trans_allUserIds :: ThentosQuery [UserId]
trans_allUserIds = do
    let label = RoleAdmin =%% False
    ThentosLabeled label . Map.keys . (^. dbUsers) <$> ask

trans_lookupUser :: UserId -> ThentosQuery (UserId, User)
trans_lookupUser uid = ask >>= \ db -> label_lookupUser $ pure_lookupUser db uid

pure_lookupUser :: DB -> UserId -> Maybe (UserId, User)
pure_lookupUser db uid =
    fmap (uid,) . Map.lookup uid $ db ^. dbUsers

label_lookupUser :: Maybe (UserId, User) -> ThentosQuery (UserId, User)
label_lookupUser (Just result@(uid, _)) = do
    let label  = RoleAdmin \/ UserA uid =%% False
    returnDb label result
label_lookupUser Nothing = do
    let label = RoleAdmin =%% False
    throwDb label NoSuchUser

trans_lookupUserByName :: UserName -> ThentosQuery (UserId, User)
trans_lookupUserByName name = ask >>= \ db -> label_lookupUser $ pure_lookupUserByName db name

-- FIXME: this is extremely inefficient, we should have a separate map
-- from user names to user ids.
pure_lookupUserByName :: DB -> UserName -> Maybe (UserId, User)
pure_lookupUserByName db name =
    find (\ (_, user) -> (user ^. userName == name)) . Map.toList . (^. dbUsers) $ db

trans_lookupUserByEmail :: UserEmail -> ThentosQuery (UserId, User)
trans_lookupUserByEmail email = ask >>= \ db -> label_lookupUser $ pure_lookupUserByEmail db email

-- FIXME: same as 'pure_lookupUserByName'.
pure_lookupUserByEmail :: DB -> UserEmail -> Maybe (UserId, User)
pure_lookupUserByEmail db email =
    find (\ (_, user) -> (user ^. userEmail == email)) . Map.toList . (^. dbUsers) $ db

-- | Write a new unconfirmed user (i.e. one whose email address we haven't
-- confirmed yet) to DB. Unlike addUser, this operation does not ensure
-- uniqueness of email adresses.
trans_addUnconfirmedUser :: Timestamp -> ConfirmationToken -> User -> ThentosUpdate (UserId, ConfirmationToken)
trans_addUnconfirmedUser now token user = do
    let label = RoleOwnsUnconfirmedUsers =%% RoleOwnsUnconfirmedUsers
    ThentosLabeled label' () <- liftThentosQuery $ assertUser label Nothing user
    uid <- freshUserId
    modify $ dbUnconfirmedUsers %~ Map.insert token (now, uid, user)
    returnDb (lub label label') (uid, token)

-- | Note on the label for this transaction: If somebody has the
-- confirmation token, we assume that she is authenticated.  Since
-- 'makeThentosClearance' does not check for confirmation tokens, this
-- transaction is publicly accessible.
trans_finishUserRegistration :: Timestamp -> Timeout -> ConfirmationToken -> ThentosUpdate UserId
trans_finishUserRegistration now expiry token = withExpiration now expiry $ do
    let label = RoleOwnsUnconfirmedUsers =%% RoleOwnsUnconfirmedUsers
    users <- gets (^. dbUnconfirmedUsers)
    case Map.lookup token users of
        Nothing -> throwDb label NoSuchPendingUserConfirmation
        Just (timestamp, uid, user) -> do
            modify $ dbUnconfirmedUsers %~ Map.delete token
            ThentosLabeled label' () <- writeUser uid user
            returnDb (lub label label') (uid, timestamp)

-- | Write new user to DB.  Return the fresh user id.
trans_addUser :: User -> ThentosUpdate UserId
trans_addUser user = do
    uid <- freshUserId
    ThentosLabeled label () <- liftThentosQuery (assertUser thentosPublic Nothing user) >> writeUser uid user
    returnDb label uid

-- | Write a list of new users to DB.  Return list of fresh user ids.
-- This is not the most vital part of the backend API, but it allows
-- for testing rollback in error cases.  It will also be a nice
-- example for intersecting authorizations.
trans_addUsers :: [User] -> ThentosUpdate [UserId]
trans_addUsers [] = returnDb thentosPublic []
trans_addUsers (u:us) = do
    ThentosLabeled l  uid  <- trans_addUser  u
    ThentosLabeled l' uids <- trans_addUsers us
    returnDb (lub l l') (uid:uids)

-- | Add a password reset token to the DB. Return the user whose password this
-- token can change.
trans_addPasswordResetToken :: Timestamp -> UserEmail -> PasswordResetToken -> ThentosUpdate User
trans_addPasswordResetToken timestamp email token = do
    let label = thentosPublic
    db <- get
    case pure_lookupUserByEmail db email of
        Nothing -> throwDb label NoSuchUser
        Just (uid, user) -> do
            modify $ dbPwResetTokens %~ Map.insert token (timestamp, uid)
            returnDb label user

-- | Change a password with a given password reset token. Throws an error if
-- the token does not exist, has already been used or has expired
trans_resetPassword :: Timestamp -> Timeout -> PasswordResetToken -> HashedSecret UserPass -> ThentosUpdate ()
trans_resetPassword now expiry token newPass = withExpiration now expiry $ do
    let label = thentosPublic
    resetTokens <- gets (^. dbPwResetTokens)
    case Map.updateLookupWithKey (const . const Nothing) token resetTokens of
        (Nothing, _) -> throwDb label NoSuchToken
        (Just (timestamp, uid), newResetTokens) -> do
            modify $ dbPwResetTokens .~ newResetTokens
            mUser <- gets $ Map.lookup uid . (^. dbUsers)
            case mUser of
                Just user -> do
                    let user' = userPassword .~ newPass $ user
                    ThentosLabeled label' () <- writeUser uid user'
                    returnDb (lub label label')  ((), timestamp)
                Nothing -> throwDb label NoSuchToken

trans_addUserEmailChangeRequest ::
    Timestamp -> UserId -> UserEmail -> ConfirmationToken -> ThentosUpdate ()
trans_addUserEmailChangeRequest timestamp uid email token = do
    let label = UserA uid =%% UserA uid
    modify $ dbEmailChangeTokens %~ Map.insert token (timestamp, uid, email)
    returnDb label ()

trans_confirmUserEmailChange :: Timestamp -> Timeout -> ConfirmationToken -> ThentosUpdate ()
trans_confirmUserEmailChange now expiry token = withExpiration now expiry $ do
    emailChangeTokens <- gets (^. dbEmailChangeTokens)
    case Map.updateLookupWithKey (const . const Nothing) token emailChangeTokens of
        (Nothing, _) -> throwDb thentosPublic NoSuchToken
        (Just (timestamp, uid, email), remainingRequests) -> do
            modify $ dbEmailChangeTokens .~ remainingRequests
            ThentosLabeled l () <- trans_updateUserField uid (UpdateUserFieldEmail email)
            returnDb l ((), timestamp)

data UpdateUserFieldOp =
    UpdateUserFieldName UserName
  | UpdateUserFieldEmail UserEmail
  | UpdateUserFieldAddService ServiceId ServiceAccount
      -- FIXME: @UpdateUserFieldAddService@ should be called
      -- @UpdateUserFieldInsertService@, as it has the same semantics
      -- as Map.insert for existing keys (overwrites).
  | UpdateUserFieldDropService ServiceId
  | UpdateUserFieldPassword (HashedSecret UserPass)
  deriving (Eq)

-- | Update existing user in DB.  Throw an error if user id does not
-- exist, or if email address in updated user is already in use by
-- another user.
trans_updateUserField :: UserId -> UpdateUserFieldOp -> ThentosUpdate ()
trans_updateUserField uid op = trans_updateUserFields uid [op]

trans_updateUserFields :: UserId -> [UpdateUserFieldOp] -> ThentosUpdate ()
trans_updateUserFields uid freeOps = do
    let runOp :: UpdateUserFieldOp -> (User -> User, Bool)
        runOp (UpdateUserFieldName n)          = (userName .~ n, True)
        runOp (UpdateUserFieldEmail e)         = (userEmail .~ e, True)
        runOp (UpdateUserFieldAddService s a)  = (userServices %~ Map.insert s a, False)
        runOp (UpdateUserFieldDropService sid) = (userServices %~ Map.filterWithKey (const . (/= sid)), False)
        runOp (UpdateUserFieldPassword p)      = (userPassword .~ p, False)

        ops :: [(User -> User, Bool)]
        ops = runOp <$> freeOps

    ThentosLabeled label (_, user) <- liftThentosQuery $ trans_lookupUser uid
    let user' = foldl' (flip fst) user ops
    _ <- if or $ snd <$> ops
            then liftThentosQuery $ assertUser thentosPublic (Just uid) user'
            else returnDb thentosPublic ()
    ThentosLabeled label' () <- writeUser uid user'
    returnDb (lub label label') ()

-- | Delete user with given user id.  If user does not exist, throw an
-- error.
trans_deleteUser :: UserId -> ThentosUpdate ()
trans_deleteUser uid = do
    let label = RoleAdmin \/ UserA uid =%% RoleAdmin /\ UserA uid
    ThentosLabeled label' (_, user) <- liftThentosQuery $ trans_lookupUser uid
    forM_ (Set.elems $ user ^. userSessions) deleteSession
    modify $ dbUsers %~ Map.delete uid
    returnDb (lub label label') ()


-- *** helpers

-- | (db ^. dbUser) must only be modified using this function.
writeUser :: UserId -> User -> ThentosUpdate ()
writeUser uid user = do
    let label = RoleAdmin \/ RoleOwnsUsers \/ UserA uid =%% RoleAdmin /\ RoleOwnsUsers /\ UserA uid
    modify $ dbUsers %~ Map.insert uid user
    returnDb label ()

assertUser :: ThentosLabel -> Maybe UserId -> User -> ThentosQuery ()
assertUser label mUid user = ask >>= \ db ->
    if | userFacetExists (^. userName)  mUid user db -> throwDb label UserNameAlreadyExists
       | userFacetExists (^. userEmail) mUid user db -> throwDb label UserEmailAlreadyExists
       | True -> returnDb label ()

userFacetExists :: Eq a => (User -> a) -> Maybe UserId -> User -> DB -> Bool
userFacetExists facet ((/=) -> notOwnUid) user db =
    (facet user `elem`) . map (facet . snd) . filter (notOwnUid . Just . fst) . Map.toList $ db ^. dbUsers


-- ** services

trans_allServiceIds :: ThentosQuery [ServiceId]
trans_allServiceIds = do
    let label = RoleAdmin =%% False
    ThentosLabeled label . Map.keys . (^. dbServices) <$> ask

trans_lookupService :: ServiceId -> ThentosQuery (ServiceId, Service)
trans_lookupService sid = do
    let label = RoleAdmin =%% False
    db <- ask
    maybe (throwDb label NoSuchService) (returnDb label) $
        pure_lookupService db sid

pure_lookupService :: DB -> ServiceId -> Maybe (ServiceId, Service)
pure_lookupService db sid = (sid,) <$> Map.lookup sid (db ^. dbServices)

-- | Write new service to DB.  Service key is generated automatically.
trans_addService :: Agent -> ServiceId -> HashedSecret ServiceKey -> ServiceName -> ServiceDescription -> ThentosUpdate ()
trans_addService owner sid key name desc = do
    let label = RoleAdmin \/ owner =%% RoleAdmin /\ owner
        service = Service key owner Nothing name desc Map.empty
    modify $ dbServices %~ Map.insert sid service
    returnDb label ()

trans_deleteService :: ServiceId -> ThentosUpdate ()
trans_deleteService sid = do
    let label = RoleAdmin \/ ServiceA sid =%% RoleAdmin /\ ServiceA sid
    ThentosLabeled label' (_, service) <- liftThentosQuery $ trans_lookupService sid
    maybe (return ()) deleteSession (service ^. serviceSession)
    modify $ dbServices %~ Map.delete sid
    returnDb (lub label label') ()


-- ** sessions

-- | Complete list of all active and inactive sessions.  This
-- transaction may go away in the future unless we can think of a good
-- use for it.
trans_allSessionTokens :: ThentosQuery [SessionToken]
trans_allSessionTokens = do
    let label = RoleAdmin =%% False
    ThentosLabeled label . Map.keys . (^. dbSessions) <$> ask


data LookupSessionResult =
    LookupSessionUnchanged (SessionToken, Session)
  | LookupSessionBumped (SessionToken, Session)
  | LookupSessionNotThere
  | LookupSessionInactive
  deriving (Eq, Ord, Show, Read)

-- | Lookup session.  Use second arg to only return session if now
-- active.  Use flag in second arg to update end-of-life 'Timestamp'
-- in result session.
pure_lookupSession :: DB -> Maybe (Timestamp, Bool) -> SessionToken -> LookupSessionResult
pure_lookupSession db mNow tok =
    let mSession :: Maybe Session = Map.lookup tok $ db ^. dbSessions
    in case (mSession, mNow) of
        (Just session, Just (now, bump)) ->
            if sessionNowActive now session
                then if bump
                    then let newEnd = Timestamp $ fromTimestamp now .+^ fromTimeout (session ^. sessionTimeout)
                         in LookupSessionBumped (tok, sessionEnd .~ newEnd $ session)
                    else LookupSessionUnchanged (tok, session)
                else LookupSessionInactive
        (Just session, Nothing) -> LookupSessionUnchanged (tok, session)
        (Nothing, _) -> LookupSessionNotThere

-- | See 'trans_lookupSession'.  The difference is that you cannot
-- bump the session, so there cannot be any changes to the database,
-- so the result monad can be 'ThentosQuery'.
trans_lookupSessionQ :: Maybe Timestamp -> SessionToken -> ThentosQuery (SessionToken, Session)
trans_lookupSessionQ mNow tok = do
    rSession <- (\ db -> pure_lookupSession db ((, False) <$> mNow) tok) <$> ask

    case rSession of
        LookupSessionUnchanged (_, session) -> do
            let label = RoleAdmin \/ (session ^. sessionAgent) =%% False
            returnDb label (tok, session)
        LookupSessionInactive -> do
            let label = RoleAdmin =%% False
            throwDb label NoSuchSession
        LookupSessionNotThere -> do
            let label = RoleAdmin =%% False
            throwDb label NoSuchSession
        LookupSessionBumped _ -> assert False $ error "trans_lookupSessionQ"


-- | Lookup session.  Use first arg to check if session is now active,
-- and if not, remove it from database.  Use flag in second arg to
-- update end-of-life 'Timestamp' in result session and database.  If
-- session does not exist, or if active check is enabled and session
-- is inactive, throw 'NoSuchSession'.
--
-- Inactive sessions are not deleted.  See
-- 'trans_GarbageCollectSessions'.
--
-- See 'trans_lookupSessionQ' for a variant of this function in
-- 'ThentosQuery'.
trans_lookupSession :: Maybe (Timestamp, Bool) -> SessionToken -> ThentosUpdate (SessionToken, Session)
trans_lookupSession mNow tok = lookupSessionAsService Nothing mNow tok


-- | Like 'trans_lookupSession', but accepts an extra 'ServiceId' as
-- first argument.  This is required for service login checks by the
-- service.
--
-- FIXME: why is this not a transaction, i.e., named starting with @trans_@?
lookupSessionAsService :: Maybe ServiceId -> Maybe (Timestamp, Bool) -> SessionToken
      -> ThentosUpdate (SessionToken, Session)
lookupSessionAsService mSid mNow tok = do
    let label = case mSid of
          Just sid -> RoleAdmin \/ ServiceA sid =%% RoleAdmin /\ ServiceA sid
          Nothing  -> RoleAdmin =%% RoleAdmin
    rSession <- (\ db -> pure_lookupSession db mNow tok) <$> get

    case rSession of
        LookupSessionUnchanged (_, session) -> do
            let label' = (session ^. sessionAgent) =%% (session ^. sessionAgent)
            returnDb (lub label label') (tok, session)
        LookupSessionBumped (_, session) -> do
            let label' = (session ^. sessionAgent) =%% (session ^. sessionAgent)
            writeSession tok session
            returnDb (lub label label') (tok, session)
        LookupSessionInactive -> throwDb label NoSuchSession
        LookupSessionNotThere -> throwDb label NoSuchSession


-- | Start a new thentos session for the given agent. Start and end time have
-- to be passed explicitly. Throw an error if the agent does not exist.
-- If the agent is a user, this new session is added to their existing sessions.
-- If the agent is a service with an existing session, its session is replaced.
trans_startSession :: SessionToken -> Agent -> Timestamp -> Timeout -> ThentosUpdate ()
trans_startSession freshSessionToken agent start lifetime = do
    let label = agent =%% agent
        session = Session agent start end lifetime Map.empty
        end = Timestamp $ fromTimestamp start .+^ fromTimeout lifetime
    ThentosLabeled _ () <- liftThentosQuery $ assertAgent agent
    writeSession freshSessionToken session
    returnDb label ()


-- | End session.  Call can be caused by logout request from user
-- (before end of session life time), by session timeouts, or after
-- its natural life time (by application's own garbage collection).
-- If lookup of session owning user fails, throw an error.
trans_endSession :: SessionToken -> ThentosUpdate ()
trans_endSession tok = do
    mSession :: Maybe Session <- Map.lookup tok . (^. dbSessions) <$> get
    let label = case mSession of
            Just session -> RoleAdmin \/ (session ^. sessionAgent) =%% RoleAdmin /\ (session ^. sessionAgent)
            Nothing      -> RoleAdmin =%% RoleAdmin

    deleteSession tok
    returnDb label ()


-- | Is session token currently active?  (Do not bump either way.)
-- Never throw errors; if session is inaccessible, simple return
-- 'False'.
--
-- Note on authorization: Since session tokens are as powerful as
-- login credentials, this transaction is labelled 'thentosPublic'.
-- (If we wanted to return 'False' for both 'PermissionDenied' and
-- 'NoSuchSession', we would have to do that in 'Action' rather than
-- in 'ThentosQuery': 'PermissionDenied' errors can only be
-- constructed from label /and/ clearance, but inside 'ThentosQuery'
-- we have no clearance value.
trans_isActiveSession :: Timestamp -> SessionToken -> ThentosQuery Bool
trans_isActiveSession now tok = do
    let label = thentosPublic
    catchT
        (trans_lookupSessionQ (Just now) tok >> returnDb label True)
        (const $ returnDb label False)


-- | Is session token currently active?  Bump if it is.  (See
-- 'trans_isActiveSession'.)
trans_isActiveSessionAndBump :: Timestamp -> SessionToken -> ThentosUpdate Bool
trans_isActiveSessionAndBump now tok = do
    let label = thentosPublic
    catchT
        (trans_lookupSession (Just (now, True)) tok >> returnDb label True)
        (const $ returnDb label False)

-- | Call 'GetServiceStatus' and return login bit.
trans_isLoggedIntoService :: Timestamp -> SessionToken -> ServiceId -> ThentosUpdate Bool
trans_isLoggedIntoService now tok sid = do
    ThentosLabeled l v <- getServiceStatus now tok sid
    returnDb l $ fromMaybe False v

-- | Call 'GetServiceStatus' and return registration bit.
trans_isRegisteredWithService :: Timestamp -> SessionToken -> ServiceId -> ThentosUpdate Bool
trans_isRegisteredWithService now tok sid = do
    ThentosLabeled l v <- getServiceStatus now tok sid
    returnDb l $ isJust v

-- | Return 'Nothing' if session owner is not registered with service,
-- or 'Just' a boolean indicating her login status if she is.  Bump session
-- if it is valid (even if user is not logged into service, but just
-- into thentos).
getServiceStatus :: Timestamp -> SessionToken -> ServiceId -> ThentosUpdate (Maybe Bool)
getServiceStatus now tok sid = do
    let label = RoleAdmin \/ ServiceA sid =%% RoleAdmin /\ ServiceA sid
    catchT
        (check >>= \ (ThentosLabeled _ v) -> returnDb label v)
        (const                             $ returnDb label Nothing)
  where
    -- Label is 'thentosDenied' so that the caller is forced to
    -- discard the label and set up a new one from scratch.
    check :: ThentosUpdate (Maybe Bool)
    check = do
        let label = thentosDenied
        ThentosLabeled _ (_, session)
            <- lookupSessionAsService (Just sid) (Just (now, True)) tok
        case session ^. sessionAgent of
            UserA uid -> do
                ThentosLabeled _ (_, user) <- liftThentosQuery $ trans_lookupUser uid
                case Map.lookup sid $ user ^. userServices of
                    Nothing -> returnDb label Nothing
                    Just _ -> returnDb label . Just $
                        let serviceSessions = session ^. sessionServiceSessions
                        in case Map.lookup sid serviceSessions of
                            Just serviceSess ->
                                (serviceSess ^. serviceSessionExpiry) > now
                            Nothing -> False
            ServiceA _ -> returnDb label Nothing

-- | Add a service login to a user session. Throws an error if the session
-- doesn't exist / has expired or is a service session. If a service session
-- already exists, it is overwritten.
trans_addServiceLogin :: Timestamp -> Timeout -> SessionToken -> ServiceId -> ThentosUpdate ()
trans_addServiceLogin now timeout tok sid = do
    let loginExpires = Timestamp $ fromTimestamp now .+^ fromTimeout timeout
    ThentosLabeled l1 (_, session) <- trans_lookupSession (Just (now, True)) tok
    case session ^. sessionAgent of
        ServiceA _ -> throwDb l1 ServiceSessionInsteadOfUserSession
        UserA uid -> do
            ThentosLabeled l2 (_, user) <- liftThentosQuery $ trans_lookupUser uid
            let label = lub l1 l2
            case Map.lookup sid (user ^. userServices) of
                Nothing -> throwDb label NotRegisteredWithService
                Just _ -> do
                    let serviceSess = ServiceSession loginExpires (user ^. userName)
                        adjustSession = sessionServiceSessions %~ Map.insert sid serviceSess
                    modify $ dbSessions %~ Map.adjust adjustSession tok
                    returnDb label ()

-- | Delete a service session. Does nothing if the session does not exist.
-- Throws an error if the session is owned by a service.
trans_dropServiceLogin :: SessionToken -> ServiceId -> ThentosUpdate ()
trans_dropServiceLogin tok sid = do
    let label' = RoleAdmin =%% RoleAdmin

        label :: Maybe Agent -> ThentosLabel
        label (Just agent) = label' `lub` agent =%% agent
        label Nothing      = label'

    mSession <- Map.lookup tok <$> gets (^. dbSessions)
    case (^. sessionAgent) <$> mSession of
        Nothing -> throwDb (label Nothing) NoSuchSession
        Just sa@(ServiceA _) ->
            throwDb (label $ Just sa) ServiceSessionInsteadOfUserSession
        Just ua@(UserA _) -> do
            let adjustSession = sessionServiceSessions %~ Map.delete sid
            modify $ dbSessions %~ Map.adjust adjustSession tok
            returnDb (label $ Just ua) ()

trans_getSessionServiceNames :: Timestamp -> SessionToken -> UserId -> ThentosQuery [ServiceName]
trans_getSessionServiceNames now tok uid = do
    ThentosLabeled label _ <- trans_lookupUser uid
    serviceMap <- (^. dbServices) <$> ask
    thentosSessions <- (^. dbSessions) <$> ask
    case Map.lookup tok thentosSessions of
        Just session -> do
            let serviceIds = map fst
                            . filter (\ (_, s) -> (s ^. serviceSessionExpiry) >= now ) $
                            Map.assocs (session ^. sessionServiceSessions)
                getServiceName sid = (^. serviceName)
                                     . fromJustNote "inconsistent db state: user has sid that is not in dbServices"
                                     $ Map.lookup sid serviceMap
                serviceNames = map getServiceName serviceIds
            returnDb label serviceNames
        Nothing -> throwDb label NoSuchSession


-- | Go through 'dbSessions' map and find all expired sessions.
-- Return in 'ThentosQuery'.  (To reduce database locking, call this
-- and then @EndSession@ on all service ids individually.)
trans_garbageCollectSessions :: ThentosQuery [SessionToken]
trans_garbageCollectSessions = assert False $ error "trans_GarbageCollectSessions: not implemented"  -- FIXME


-- | FIXME: implement this to the point where it is visible as
-- metadata in helloworld.  (don't worry too much about authorization
-- / authentication for now.)
trans_getServiceSessionMetaData :: SessionToken -> ThentosQuery ServiceSession
trans_getServiceSessionMetaData = error "trans_getServiceSessionMetaData"


-- *** helpers

sessionNowActive :: Timestamp -> Session -> Bool
sessionNowActive now session = (session ^. sessionStart) < now && now < (session ^. sessionEnd)


-- | Write session to database (both in 'dbSessions' and in the
-- 'Agent').  If first arg is given and 'Agent' is a user, update
-- service login list of this user, too.
writeSession :: SessionToken -> Session -> ThentosUpdate' e ()
writeSession tok session = do
    modify $ dbSessions %~ Map.insert tok session
    modify $ case session ^. sessionAgent of
        UserA    uid -> dbUsers    %~ Map.adjust _updateUser uid
        ServiceA sid -> dbServices %~ Map.adjust _updateService sid
    return ()
  where
    _updateUser :: User -> User
    _updateUser = userSessions %~ Set.insert tok

    _updateService :: Service -> Service
    _updateService = serviceSession .~ Just tok

-- | Remove session from the database (both from 'dbSessions' and the agent's
-- sessions)
deleteSession :: SessionToken -> ThentosUpdate' e ()
deleteSession tok = do
    mSession :: Maybe Session <- Map.lookup tok . (^. dbSessions) <$> get
    case (^. sessionAgent) <$> mSession of
        Just (UserA    uid) -> modify $ dbUsers    %~ Map.adjust _updateUser uid
        Just (ServiceA sid) -> modify $ dbServices %~ Map.adjust _updateService sid
        Nothing             -> return ()

    modify $ dbSessions %~ Map.delete tok
    return ()
  where
    _updateUser :: User -> User
    _updateUser = userSessions %~ Set.delete tok

    _updateService :: Service -> Service
    _updateService = serviceSession .~ Nothing


-- * agents and roles

-- | Extend 'Agent's entry in 'dbRoles' with a new 'Role'.  If 'Role'
-- is already assigned to 'Agent', do nothing.  If Agent does not
-- point to an existing entry in user or service table (or such),
-- throw an error.
trans_assignRole :: Agent -> Role -> ThentosUpdate ()
trans_assignRole agent role = do
    let label = RoleAdmin =%% RoleAdmin
    ThentosLabeled _ () <- liftThentosQuery $ assertAgent agent
    let inject = Just . (role:) . fromMaybe []
    modify $ dbRoles %~ Map.alter inject agent
    returnDb label ()

-- | Extend 'Agent's entry in 'dbRoles' with a new 'Role'.  If 'Role'
-- is not assigned to 'Agent', do nothing.  If Agent does not
-- point to an existing entry in user or service table (or such),
-- throw an error.
trans_unassignRole :: Agent -> Role -> ThentosUpdate ()
trans_unassignRole agent role = do
    let label = RoleAdmin =%% RoleAdmin
    ThentosLabeled _ () <- liftThentosQuery $ assertAgent agent
    let exject = fmap (\\ [role])
    modify $ dbRoles %~ Map.alter exject agent
    returnDb label ()

-- | All 'Role's of an 'Agent'.  If 'Agent' does not exist or
-- has no entry in 'dbRoles', return an empty list.
trans_lookupAgentRoles :: Agent -> ThentosQuery [Role]
trans_lookupAgentRoles agent = do
    let label = agent \/ RoleAdmin =%% False
    ThentosLabeled label . (`pure_lookupAgentRoles` agent) <$> ask

pure_lookupAgentRoles :: DB -> Agent -> [Role]
pure_lookupAgentRoles db agent = fromMaybe [] $ Map.lookup agent (db ^. dbRoles)


-- *** helpers

-- | 'assertAgent' is only used to build transactions, and is not a
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
    label = thentosDenied

    f :: Agent -> ThentosQuery ()
    f (UserA    uid) = ask >>= g . Map.lookup uid . (^. dbUsers)
    f (ServiceA sid) = ask >>= g . Map.lookup sid . (^. dbServices)

    g :: Maybe a -> ThentosQuery ()
    g (Just _) = returnDb label ()
    g Nothing  = throwDb label NoSuchUser


-- ** misc

trans_snapShot :: ThentosQuery DB
trans_snapShot = do
    let label = RoleAdmin =%% False
    ask >>= returnDb label

-- | Token expiration handling
withExpiration :: Timestamp -> Timeout -> ThentosUpdate (a, Timestamp) -> ThentosUpdate a
withExpiration now expiryPeriod action = do
    ThentosLabeled l (result, tokenCreationTime) <- action
    if fromTimestamp tokenCreationTime .+^ fromTimeout expiryPeriod < fromTimestamp now
        then throwDb l NoSuchToken
        else returnDb l result


-- * event types

-- FIXME: this section should be entirely constructed by TemplateHaskell

allUserIds :: ThentosClearance -> Query DB (Either ThentosError [UserId])
allUserIds clearance = runThentosQuery clearance trans_allUserIds

lookupUser :: UserId -> ThentosClearance -> Query DB (Either ThentosError (UserId, User))
lookupUser uid clearance = runThentosQuery clearance $ trans_lookupUser uid

lookupUserByName :: UserName -> ThentosClearance -> Query DB (Either ThentosError (UserId, User))
lookupUserByName name clearance = runThentosQuery clearance $ trans_lookupUserByName name

lookupUserByEmail :: UserEmail -> ThentosClearance -> Query DB (Either ThentosError (UserId, User))
lookupUserByEmail email clearance = runThentosQuery clearance $ trans_lookupUserByEmail email

addUnconfirmedUser :: Timestamp -> ConfirmationToken -> User -> ThentosClearance -> Update DB (Either ThentosError (UserId, ConfirmationToken))
addUnconfirmedUser now token user clearance =
    runThentosUpdate clearance $ trans_addUnconfirmedUser now token user

finishUserRegistration :: Timestamp -> Timeout -> ConfirmationToken -> ThentosClearance -> Update DB (Either ThentosError UserId)
finishUserRegistration now expiry token clearance =
    runThentosUpdate clearance $ trans_finishUserRegistration now expiry token

addUser :: User -> ThentosClearance -> Update DB (Either ThentosError UserId)
addUser user clearance = runThentosUpdate clearance $ trans_addUser user

addUsers :: [User] -> ThentosClearance -> Update DB (Either ThentosError [UserId])
addUsers users clearance = runThentosUpdate clearance $ trans_addUsers users

addUserEmailChangeRequest :: Timestamp -> UserId -> UserEmail -> ConfirmationToken -> ThentosClearance -> Update DB (Either ThentosError ())
addUserEmailChangeRequest now uid email token clearance = runThentosUpdate clearance $ trans_addUserEmailChangeRequest now uid email token

confirmUserEmailChange :: Timestamp -> Timeout -> ConfirmationToken -> ThentosClearance -> Update DB (Either ThentosError ())
confirmUserEmailChange now expiry token clearance = runThentosUpdate clearance $ trans_confirmUserEmailChange now expiry token

addPasswordResetToken :: Timestamp -> UserEmail -> PasswordResetToken -> ThentosClearance -> Update DB (Either ThentosError User)
addPasswordResetToken timestamp email token clearance = runThentosUpdate clearance $ trans_addPasswordResetToken timestamp email token

resetPassword :: Timestamp -> Timeout -> PasswordResetToken -> HashedSecret UserPass -> ThentosClearance -> Update DB (Either ThentosError ())
resetPassword timestamp expiry token newPass clearance = runThentosUpdate clearance $ trans_resetPassword timestamp expiry token newPass

updateUserField :: UserId -> UpdateUserFieldOp -> ThentosClearance -> Update DB (Either ThentosError ())
updateUserField uid op clearance = runThentosUpdate clearance $ trans_updateUserField uid op

updateUserFields :: UserId -> [UpdateUserFieldOp] -> ThentosClearance -> Update DB (Either ThentosError ())
updateUserFields uid ops clearance = runThentosUpdate clearance $ trans_updateUserFields uid ops

deleteUser :: UserId -> ThentosClearance -> Update DB (Either ThentosError ())
deleteUser uid clearance = runThentosUpdate clearance $ trans_deleteUser uid

allServiceIds :: ThentosClearance -> Query DB (Either ThentosError [ServiceId])
allServiceIds clearance = runThentosQuery clearance trans_allServiceIds

lookupService :: ServiceId -> ThentosClearance -> Query DB (Either ThentosError (ServiceId, Service))
lookupService sid clearance = runThentosQuery clearance $ trans_lookupService sid

addService :: Agent -> ServiceId -> HashedSecret ServiceKey -> ServiceName -> ServiceDescription
    -> ThentosClearance -> Update DB (Either ThentosError ())
addService owner sid key name desc clearance = runThentosUpdate clearance $ trans_addService owner sid key name desc

deleteService :: ServiceId -> ThentosClearance -> Update DB (Either ThentosError ())
deleteService sid clearance = runThentosUpdate clearance $ trans_deleteService sid

allSessionTokens :: ThentosClearance -> Query DB (Either ThentosError [SessionToken])
allSessionTokens clearance = runThentosQuery clearance trans_allSessionTokens

lookupSessionQ :: Maybe Timestamp -> SessionToken -> ThentosClearance -> Query DB (Either ThentosError (SessionToken, Session))
lookupSessionQ mNow tok clearance = runThentosQuery clearance $ trans_lookupSessionQ mNow tok

lookupSession :: Maybe (Timestamp, Bool) -> SessionToken -> ThentosClearance -> Update DB (Either ThentosError (SessionToken, Session))
lookupSession mNow tok clearance = runThentosUpdate clearance $ trans_lookupSession mNow tok

startSession :: SessionToken -> Agent -> Timestamp -> Timeout -> ThentosClearance -> Update DB (Either ThentosError ())
startSession tok agent start lifetime clearance = runThentosUpdate clearance $ trans_startSession tok agent start lifetime

endSession :: SessionToken -> ThentosClearance -> Update DB (Either ThentosError ())
endSession tok clearance = runThentosUpdate clearance $ trans_endSession tok

isActiveSession :: Timestamp -> SessionToken -> ThentosClearance -> Query DB (Either ThentosError Bool)
isActiveSession now tok clearance = runThentosQuery clearance $ trans_isActiveSession now tok

isActiveSessionAndBump :: Timestamp -> SessionToken -> ThentosClearance -> Update DB (Either ThentosError Bool)
isActiveSessionAndBump now tok clearance = runThentosUpdate clearance $ trans_isActiveSessionAndBump now tok

isLoggedIntoService :: Timestamp -> SessionToken -> ServiceId -> ThentosClearance -> Update DB (Either ThentosError Bool)
isLoggedIntoService now tok sid clearance = runThentosUpdate clearance $ trans_isLoggedIntoService now tok sid

isRegisteredWithService :: Timestamp -> SessionToken -> ServiceId -> ThentosClearance -> Update DB (Either ThentosError Bool)
isRegisteredWithService now tok sid clearance = runThentosUpdate clearance $ trans_isRegisteredWithService now tok sid

addServiceLogin :: Timestamp -> Timeout -> SessionToken -> ServiceId -> ThentosClearance -> Update DB (Either ThentosError ())
addServiceLogin now timeout tok sid clearance = runThentosUpdate clearance $ trans_addServiceLogin now timeout tok sid

dropServiceLogin :: SessionToken -> ServiceId -> ThentosClearance -> Update DB (Either ThentosError ())
dropServiceLogin tok sid clearance = runThentosUpdate clearance $ trans_dropServiceLogin tok sid

getSessionServiceNames :: Timestamp -> SessionToken -> UserId -> ThentosClearance -> Query DB (Either ThentosError [ServiceName])
getSessionServiceNames now tok uid clearance = runThentosQuery clearance $ trans_getSessionServiceNames now tok uid

garbageCollectSessions :: ThentosClearance -> Query DB (Either ThentosError [SessionToken])
garbageCollectSessions clearance = runThentosQuery clearance $ trans_garbageCollectSessions

assignRole :: Agent -> Role -> ThentosClearance -> Update DB (Either ThentosError ())
assignRole agent role clearance = runThentosUpdate clearance $ trans_assignRole agent role

unassignRole :: Agent -> Role -> ThentosClearance -> Update DB (Either ThentosError ())
unassignRole agent role clearance = runThentosUpdate clearance $ trans_unassignRole agent role

lookupAgentRoles :: Agent -> ThentosClearance -> Query DB (Either ThentosError [Role])
lookupAgentRoles agent clearance = runThentosQuery clearance $ trans_lookupAgentRoles agent

snapShot :: ThentosClearance -> Query DB (Either ThentosError DB)
snapShot clearance = runThentosQuery clearance trans_snapShot


$(deriveSafeCopy 0 'base ''UpdateUserFieldOp)

$(makeAcidic ''DB
    [ 'allUserIds
    , 'lookupUser
    , 'lookupUserByName
    , 'lookupUserByEmail
    , 'addUser
    , 'addUnconfirmedUser
    , 'finishUserRegistration
    , 'addUsers
    , 'updateUserField
    , 'updateUserFields
    , 'deleteUser
    , 'addUserEmailChangeRequest
    , 'confirmUserEmailChange
    , 'addPasswordResetToken
    , 'resetPassword

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
    , 'isRegisteredWithService
    , 'addServiceLogin
    , 'dropServiceLogin
    , 'getSessionServiceNames
    , 'garbageCollectSessions

    , 'assignRole
    , 'unassignRole
    , 'lookupAgentRoles

    , 'snapShot
    ])
