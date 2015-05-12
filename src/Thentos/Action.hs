{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PackageImports       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}

module Thentos.Action
where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar (MVar, newMVar)
import Control.Exception (finally)
import Control.Lens ((^.))
import Control.Monad.Except (throwError, catchError)
import "crypto-random" Crypto.Random (SystemRNG, createEntropyPool, cprgCreate)
import Data.Acid (AcidState, openLocalStateFrom, createCheckpoint, closeAcidState)
import Data.Configifier ((>>.), Tagged(Tagged))
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.Set (Set)
import Data.String.Conversions (ST, cs)
import LIO.Core (liftLIO, setLabel)
import LIO.DCLabel ((%%))

import qualified Codec.Binary.Base64 as Base64
import qualified Data.Map as Map
import qualified Data.Set as Set

import Thentos.Action.Core
import Thentos.Config
import Thentos.Types
import Thentos.Util

import qualified Thentos.Transaction as T


-- * randomness

-- | Return a base64 encoded random string of length 24 (18 bytes of entropy).
freshRandomName :: Action DB ST
freshRandomName = cs . Base64.encode <$> genRandomBytes'P 18

freshConfirmationToken :: Action DB ConfirmationToken
freshConfirmationToken = ConfirmationToken <$> freshRandomName

freshPasswordResetToken :: Action DB PasswordResetToken
freshPasswordResetToken = PasswordResetToken <$> freshRandomName

freshServiceId :: Action DB ServiceId
freshServiceId = ServiceId <$> freshRandomName

freshServiceKey :: Action DB ServiceKey
freshServiceKey = ServiceKey <$> freshRandomName

freshSessionToken :: Action DB ThentosSessionToken
freshSessionToken = ThentosSessionToken <$> freshRandomName

freshServiceSessionToken :: Action DB ServiceSessionToken
freshServiceSessionToken = ServiceSessionToken <$> freshRandomName


-- * user

lookupUser :: UserId -> Action DB (UserId, User)
lookupUser = query'P . T.LookupUser

lookupUserByName :: UserName -> Action DB (UserId, User)
lookupUserByName = query'P . T.LookupUserByName

lookupUserByEmail :: UserEmail -> Action DB (UserId, User)
lookupUserByEmail = query'P . T.LookupUserByEmail

addUnconfirmedUser :: UserFormData -> Action DB (UserId, ConfirmationToken)
addUnconfirmedUser userData = do
    now <- getCurrentTime'P
    tok <- freshConfirmationToken
    user <- makeUserFromFormData'P userData
    update'P $ T.AddUnconfirmedUser now tok user

confirmNewUser :: ConfirmationToken -> Action DB UserId
confirmNewUser token = do
    expiryPeriod <- (>>. (Proxy :: Proxy '["user_reg_expiration"])) <$> getConfig'P
    now <- getCurrentTime'P
    update'P $ T.FinishUserRegistration now expiryPeriod token

addPasswordResetToken :: UserEmail -> Action DB (User, PasswordResetToken)
addPasswordResetToken email = do
    now <- getCurrentTime'P
    tok <- freshPasswordResetToken
    user <- update'P $ T.AddPasswordResetToken now email tok
    return (user, tok)

resetPassword :: PasswordResetToken -> UserPass -> Action DB ()
resetPassword token password = do
    now <- getCurrentTime'P
    expiryPeriod <- (>>. (Proxy :: Proxy '["pw_reset_expiration"])) <$> getConfig'P
    hashedPassword <- hashUserPass'P password
    update'P $ T.ResetPassword now expiryPeriod token hashedPassword

changePassword :: UserId -> UserPass -> UserPass -> Action DB ()
changePassword uid old new = do
    _ <- findUserCheckPassword (query'P $ T.LookupUser uid) old
    hashedPw <- hashUserPass'P new
    update'P $ T.UpdateUserField uid (T.UpdateUserFieldPassword hashedPw)

-- | Find user running the action, confirm the password, and return the user or crash.  'NoSuchUser'
-- is translated into 'BadCredentials'.
findUserCheckPassword :: Action DB (UserId, User) -> UserPass -> Action DB (UserId, User)
findUserCheckPassword action password = a `catchError` h
  where
    a = do
        (uid, user) <- action
        if verifyPass password user
            then return (uid, user)
            else throwError BadCredentials

    h NoSuchUser = throwError BadCredentials
    h e          = throwError e

requestUserEmailChange :: UserId -> UserEmail -> (ConfirmationToken -> ST) -> Action DB ()
requestUserEmailChange uid newEmail callbackUrlBuilder = do
    -- restrictWrite [UserA uid]

    tok <- freshConfirmationToken
    now <- getCurrentTime'P
    smtpConfig <- (Tagged . (>>. (Proxy :: Proxy '["smtp"]))) <$> getConfig'P

    update'P $ T.AddUserEmailChangeRequest now uid newEmail tok

    let message = "Please go to " <> callbackUrlBuilder tok <> " to confirm your change of email address."
        subject = "Thentos email address change"
    sendMail'P smtpConfig Nothing newEmail subject message

-- | Look up the given confirmation token and updates the user's email address iff 1) the token
-- exists, 2) the token belongs to the user, and 3) the token has not expired. If any of these
-- conditions don't apply, throw 'NoSuchToken' to avoid leaking information.
--
-- FIXME: set label so that only the logged-in user can find the token.  then catch 'permission
-- denied' and translate into 'no such token'.
confirmUserEmailChange :: ConfirmationToken -> Action DB ()
confirmUserEmailChange token = do
    now <- getCurrentTime'P
    expiryPeriod <- (>>. (Proxy :: Proxy '["email_change_expiration"])) <$> getConfig'P
    update'P $ T.ConfirmUserEmailChange now expiryPeriod token

updateUserField :: UserId -> T.UpdateUserFieldOp -> Action DB ()
updateUserField uid = update'P . T.UpdateUserField uid

updateUserFields :: UserId -> [T.UpdateUserFieldOp] -> Action DB ()
updateUserFields uid = update'P . T.UpdateUserFields uid


-- * service

lookupService :: ServiceId -> Action DB (ServiceId, Service)
lookupService = query'P . T.LookupService

addService :: Agent -> ServiceName -> ServiceDescription -> Action DB (ServiceId, ServiceKey)
addService owner name desc = do
    -- restrictWrite [owner]

    sid <- freshServiceId
    key <- freshServiceKey
    hashedKey <- hashServiceKey'P key
    update'P $ T.AddService owner sid hashedKey name desc
    return (sid, key)

-- | List all group leafs a user is member in on some service.
userGroups :: UserId -> ServiceId -> Action DB [Group]
userGroups uid sid = do
    -- restrictRead [UserA uid, ServiceA sid]

    (_, service) <- query'P $ T.LookupService sid

    let groupMap :: Map.Map GroupNode (Set Group)
        groupMap = service ^. serviceGroups

        memberships :: GroupNode -> Set Group
        memberships g = Map.findWithDefault Set.empty g groupMap

        unionz :: Set (Set Group) -> Set Group
        unionz = Set.fold Set.union Set.empty

        f :: GroupNode -> Set Group
        f g@(GroupU _) =                r g
        f g@(GroupG n) = n `Set.insert` r g

        r :: GroupNode -> Set Group
        r g = unionz $ Set.map (f . GroupG) (memberships g)

    return . Set.toList . f . GroupU $ uid


-- * thentos session

-- | Thentos and service sessions have a fixed duration of 2 weeks.
--
-- FIXME: make configurable, and distinguish between thentos sessions and service sessions.
-- (eventually, this will need to be run-time configurable.  but there will probably still be global
-- defaults handled by configifier.)
defaultSessionTimeout :: Timeout
defaultSessionTimeout = Timeout $ 14 * 24 * 3600

lookupThentosSession :: ThentosSessionToken -> Action DB ThentosSession
lookupThentosSession tok = do
    now <- getCurrentTime'P
    snd <$> update'P (T.LookupThentosSession now tok)

-- | Like 'lookupThentosSession', but (1) does not throw exceptions and (2) returns less information
-- and therefore can have a more liberal label.
existsThentosSession :: ThentosSessionToken -> Action DB Bool
existsThentosSession tok = (lookupThentosSession tok >> return True) `catchError`
    \case NoSuchThentosSession -> return False
          e                    -> throwError e

-- | Check user credentials and create a session for user.
startThentosSessionByUserId :: UserId -> UserPass -> Action DB ThentosSessionToken
startThentosSessionByUserId uid pass = do
    _ <- findUserCheckPassword (query'P $ T.LookupUser uid) pass
    startThentosSessionByAgent (UserA uid)

startThentosSessionByUserName :: UserName -> UserPass -> Action DB (UserId, ThentosSessionToken)
startThentosSessionByUserName name pass = do
    (uid, _) <- findUserCheckPassword (query'P $ T.LookupUserByName name) pass
    (uid,) <$> startThentosSessionByAgent (UserA uid)

-- | Check service credentials and create a session for service.
startThentosSessionByServiceId :: ServiceId -> ServiceKey -> Action DB ThentosSessionToken
startThentosSessionByServiceId sid key = do
    _ <- findServiceCheckKey (query'P $ T.LookupService sid) key
    startThentosSessionByAgent (ServiceA sid)

endThentosSession :: ThentosSessionToken -> Action DB ()
endThentosSession = update'P . T.EndThentosSession

-- | Like 'findUserCheckPassword'.
findServiceCheckKey :: Action DB (ServiceId, Service) -> ServiceKey -> Action DB (ServiceId, Service)
findServiceCheckKey action key = a `catchError` h
  where
    a = do
        (sid, service) <- action
        if verifyKey key service
            then return (sid, service)
            else throwError BadCredentials

    h NoSuchService = throwError BadCredentials
    h e             = throwError e

-- | Open a session for any agent.  This can only be called if you -- no, that's not how this works?!
startThentosSessionByAgent :: Agent -> Action DB ThentosSessionToken
startThentosSessionByAgent agent = do
    -- restrictTotal

    now <- getCurrentTime'P
    tok <- freshSessionToken
    update'P $ T.StartThentosSession tok agent now defaultSessionTimeout
    return tok


-- | For a thentos session, look up all service sessions and return their service names.
serviceNamesFromThentosSession :: ThentosSessionToken -> Action DB [ServiceName]
serviceNamesFromThentosSession tok = do
    now <- getCurrentTime'P
    ts :: Set.Set ServiceSessionToken <- (^. thSessServiceSessions) . snd <$> update'P (T.LookupThentosSession now tok)
    ss :: [ServiceSession]            <- mapM (fmap snd . update'P . T.LookupServiceSession now) $ Set.toList ts
    xs :: [(ServiceId, Service)]      <- mapM (\ s -> query'P $ T.LookupService (s ^. srvSessService)) ss

    return $ (^. serviceName) . snd <$> xs


-- * service session

lookupServiceSession :: ServiceSessionToken -> Action DB ServiceSession
lookupServiceSession tok = do
    now <- getCurrentTime'P
    snd <$> update'P (T.LookupServiceSession now tok)

existsServiceSession :: ServiceSessionToken -> Action DB Bool
existsServiceSession tok = (lookupServiceSession tok >> return True) `catchError`
    \case NoSuchServiceSession -> return False
          e                    -> throwError e

thentosSessionAndUserIdByToken :: ThentosSessionToken -> Action DB (ThentosSession, UserId)
thentosSessionAndUserIdByToken tok = do
    session <- lookupThentosSession tok
    case session ^. thSessAgent of
        UserA uid -> return (session, uid)
        ServiceA sid -> throwError $ NeedUserA tok sid

addServiceRegistration :: ThentosSessionToken -> ServiceId -> Action DB ()
addServiceRegistration tok sid = do
    (_, uid) <- thentosSessionAndUserIdByToken tok
    update'P $ T.UpdateUserField uid (T.UpdateUserFieldInsertService sid newServiceAccount)

dropServiceRegistration :: ThentosSessionToken -> ServiceId -> Action DB ()
dropServiceRegistration tok sid = do
    (_, uid) <- thentosSessionAndUserIdByToken tok
    update'P $ T.UpdateUserField uid (T.UpdateUserFieldDropService sid)

-- | If user is not registered, throw an error.
startServiceSession :: ThentosSessionToken -> ServiceId -> Action DB ServiceSessionToken
startServiceSession ttok sid = do
    now <- getCurrentTime'P
    stok <- freshServiceSessionToken
    update'P $ T.StartServiceSession ttok stok sid now defaultSessionTimeout
    return stok

-- | If user is not registered, throw an error.
endServiceSession :: ServiceSessionToken -> Action DB ()
endServiceSession = update'P . T.EndServiceSession

getServiceSessionMetadata :: ServiceSessionToken -> Action DB ServiceSessionMetadata
getServiceSessionMetadata tok = (^. srvSessMetadata) <$> lookupServiceSession tok


-- * agents and roles

assignRole :: Agent -> Role -> Action DB ()
assignRole agent = update'P . T.AssignRole agent

unassignRole :: Agent -> Role -> Action DB ()
unassignRole agent = update'P . T.UnassignRole agent

agentRoles :: Agent -> Action DB [Role]
agentRoles = fmap Set.toList . query'P . T.AgentRoles


-- * garbage collection

collectGarbage :: Action DB ()
collectGarbage = do
    now <- getCurrentTime'P
    query'P (T.GarbageCollectThentosSessions now) >>= update'P . T.DoGarbageCollectThentosSessions
    query'P (T.GarbageCollectServiceSessions now) >>= update'P . T.DoGarbageCollectServiceSessions

    config <- getConfig'P
    let userExpiry = config >>. (Proxy :: Proxy '["user_reg_expiration"])
        passwordExpiry = config >>. (Proxy :: Proxy '["pw_reset_expiration"])
        emailExpiry = config >>. (Proxy :: Proxy '["email_change_expiration"])
    update'P $ T.DoGarbageCollectUnconfirmedUsers now userExpiry
    update'P $ T.DoGarbageCollectEmailChangeTokens now emailExpiry
    update'P $ T.DoGarbageCollectPasswordResetTokens now passwordExpiry


-- XXX: something like this should go to the test suite...
main' :: IO ()
main' =
  do
    st     :: AcidState DB   <- openLocalStateFrom ".acid-state/" emptyDB
    rng    :: MVar SystemRNG <- createEntropyPool >>= newMVar . cprgCreate
    config :: ThentosConfig  <- getConfig "devel.config"

    let clearance = False %% False

    flip finally (createCheckpoint st >> closeAcidState st) $ do
        result <- runActionE clearance (ActionState (st, rng, config)) $ do
            liftLIO $ setLabel $ True %% True
            query'P $ T.LookupUser (UserId 0)
        print result



-- $overview
--
-- Thentos distinguishes between /transactions/ and /actions/.
--
-- /transactions/ are acidic in the sense used in acid-state, and live
-- in 'ThentosQuery' or 'ThentosUpdate', which are monad transformer
-- stacks over the corresponding acid-state types.  On top of
-- acid-state access, thentos transactions provide error handling and
-- authorization management.
--
-- /actions/ can be composed of more than one acidic transaction in a
-- non-acidic fashion, and possibly do other things involving
-- randomness or the system time.  Actions live in the 'Action' monad
-- transformer stack and provide access to acid-state as well as 'IO',
-- and authentication management.  'query'P' and 'update'P'
-- can be used to translate transactions into actions.
--
-- A collection of basic transactions is implemented in "Thentos.DB.Trans",
-- and one of simple actions is implemented in "Thentos.Api".  Software using
-- Thentos as a library is expected to add more transactions and
-- actions and place them in other modules.


-- $authorization
--
-- Access to acid-state data is protected by `DCLabel`s from the lio
-- package.  Each transaction is called in the context of a clearance
-- level ('ThentosClearance') that describes the privileges of the
-- calling user.  Each transaction returns its result together with a
-- label ('ThentosLabel').  The label must satisfy @label `canFlowTo`
-- clearance@ (see 'canFlowTo').  The functions 'accessAction',
-- 'update'P', 'query'P' take the clearance level from the
-- application state and attach it to the acid-state event.
--
-- Labels can be combined to a least upper bound (method 'lub' of type
-- class 'Label').  If several transactions are combined to a new,
-- more complex transaction, the latter must be labeled with the least
-- upper bound of the more primitive ones.
--
-- In order to avoid having to check label against clearance
-- explicitly inside every transaction, we use a Thentos-specific
-- derivative of 'makeAcidic' that calls 'Thentos.DB.Core.runThentosUpdate' or
-- 'Thentos.DB.Core.runThentosQuery', resp..  (FIXME: not implemented yet!)
--
-- If you need to implement an action that runs with higher clearance
-- than the current user can present, 'accessAction' takes a 'Maybe'
-- argument to override the presented clearance.
--
-- See the (admittedly rather sparse) lio documentation for more
-- details (or ask the thentos maintainers to elaborate on this :-).
