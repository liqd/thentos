{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE DeriveDataTypeable                       #-}
{-# LANGUAGE DeriveFunctor                            #-}
{-# LANGUAGE DeriveGeneric                            #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving               #-}
{-# LANGUAGE OverlappingInstances                     #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TemplateHaskell                          #-}
{-# LANGUAGE UndecidableInstances                     #-}

{-# OPTIONS -fno-warn-orphans #-}

module Thentos.Types where

import Control.Lens (makeLenses)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Typeable)
import Data.Functor.Infix ((<$>))
import Data.Map (Map)
import Data.SafeCopy (SafeCopy, deriveSafeCopy, base, contain, putCopy, getCopy, safePut, safeGet)
import Data.String.Conversions (ST)
import Data.String (IsString)
import Data.Thyme (UTCTime, NominalDiffTime, formatTime, parseTime, toSeconds, fromSeconds)
import GHC.Generics (Generic)
import LIO.DCLabel (DCLabel, ToCNF, toCNF)
import LIO.Label (Label, canFlowTo, glb, lub)
import Safe (readMay)
import Servant.Common.Text (FromText)
import System.Locale (defaultTimeLocale)
import System.Log.Logger (Priority(INFO))

import qualified Crypto.Scrypt as Scrypt
import qualified Data.Aeson as Aeson
import qualified Generics.Generic.Aeson as Aeson

import System.Log.Missing (logger)


-- * db

data DB =
    DB
      { _dbUsers            :: Map UserId User
      , _dbUnconfirmedUsers :: Map ConfirmationToken (UserId, User)
      , _dbServices         :: Map ServiceId Service
      , _dbSessions         :: Map SessionToken Session
      , _dbRoles            :: Map Agent [Role]
      , _dbPwResetTokens    :: Map PasswordResetToken (TimeStamp, UserId)
      , _dbFreshUserId      :: !UserId
      }
  deriving (Eq, Show, Typeable, Generic)


-- * user

-- | (user groups (the data that services want to store and retrieve
-- in thentos) and session tokens of all active sessions are stored in
-- assoc lists rather than maps.  this saves us explicit json
-- instances for now.)
data User =
    User
      { _userName     :: !UserName
      , _userPassword :: !(HashedSecret UserPass)
      , _userEmail    :: !UserEmail
      , _userGroups   :: [(ServiceId, [Group])]
      , _userSession  :: !(Maybe SessionToken)
      , _userLogins   :: [ServiceId]
      }
  deriving (Eq, Show, Typeable, Generic)

newtype UserId = UserId { fromUserId :: Integer }
    deriving (Eq, Ord, Enum, Show, Read, FromJSON, ToJSON, Typeable, Generic, FromText)

newtype UserName = UserName { fromUserName :: ST }
    deriving (Eq, Ord, Show, Read, FromJSON, ToJSON, Typeable, Generic, IsString)

-- | FIXME: ToJSON instance should go away in order to avoid
-- accidental leakage of cleartext passwords.  but for the
-- experimentation phase this is too much of a headache.  either way,
-- don't do any half-assed rendering to "[password hidden]".  causes
-- too many confusing errors.
newtype UserPass = UserPass { fromUserPass :: ST }
    deriving (Eq, FromJSON, ToJSON, Typeable, Generic, IsString)

newtype HashedSecret a = HashedSecret { fromHashedSecret :: Scrypt.EncryptedPass }
    deriving (Eq, Show, Typeable, Generic)

instance SafeCopy (HashedSecret a) where
    putCopy = contain . safePut . Scrypt.getEncryptedPass . fromHashedSecret
    getCopy = contain $ safeGet >>= return . HashedSecret . Scrypt.EncryptedPass

newtype UserEmail = UserEmail { fromUserEmail :: ST }
    deriving (Eq, Ord, FromJSON, ToJSON, Show, Read, Typeable, Generic, IsString)

newtype Group = Group { fromGroup :: ST }
    deriving (Eq, Ord, Show, Read, Typeable, Generic, IsString)

newtype ConfirmationToken = ConfirmationToken { fromConfimationToken :: ST }
    deriving (Eq, Ord, Show, Read, Typeable, Generic)

newtype PasswordResetToken = PasswordResetToken { fromPasswordResetToken :: ST }
    deriving (Eq, Ord, Show, Read, Typeable, Generic)

-- | Information required to create a new User
data UserFormData =
    UserFormData
        { udName     :: !UserName
        , udPassword :: !UserPass
        , udEmail    :: !UserEmail
        }
    deriving (Eq, Typeable, Generic)

instance Aeson.FromJSON UserFormData where parseJSON = Aeson.gparseJson
instance Aeson.ToJSON UserFormData where toJSON = Aeson.gtoJson


-- * service

data Service =
    Service
      { _serviceKey         :: !(HashedSecret ServiceKey)
      , _serviceSession     :: !(Maybe SessionToken)
      , _serviceName        :: !ServiceName
      , _serviceDescription :: !ServiceDescription
      }
  deriving (Eq, Show, Typeable, Generic)

newtype ServiceId = ServiceId { fromServiceId :: ST }
  deriving (Eq, Ord, Show, Read, Typeable, Generic, IsString, FromText)

instance Aeson.FromJSON ServiceId where parseJSON = Aeson.gparseJson
instance Aeson.ToJSON ServiceId where toJSON = Aeson.gtoJson

newtype ServiceKey = ServiceKey { fromServiceKey :: ST }
  deriving (Eq, Ord, Show, Read, Typeable, Generic, IsString)

instance Aeson.FromJSON ServiceKey where parseJSON = Aeson.gparseJson
instance Aeson.ToJSON ServiceKey where toJSON = Aeson.gtoJson

newtype ServiceName = ServiceName { fromServiceName :: ST }
  deriving (Eq, Ord, Show, Read, Typeable, Generic, IsString, FromText)

instance Aeson.FromJSON ServiceName where parseJSON = Aeson.gparseJson
instance Aeson.ToJSON ServiceName where toJSON = Aeson.gtoJson

newtype ServiceDescription = ServiceDescription { fromServiceDescription :: ST }
  deriving (Eq, Ord, Show, Read, Typeable, Generic, IsString, FromText)

instance Aeson.FromJSON ServiceDescription where parseJSON = Aeson.gparseJson
instance Aeson.ToJSON ServiceDescription where toJSON = Aeson.gtoJson

-- * session, timestamp, timeout

data Session =
    Session
      { _sessionAgent   :: !Agent
      , _sessionStart   :: !TimeStamp
      , _sessionEnd     :: !TimeStamp
      , _sessionTimeout :: !Timeout
      }
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance Aeson.FromJSON Session where parseJSON = Aeson.gparseJson
instance Aeson.ToJSON Session where toJSON = Aeson.gtoJson

newtype SessionToken = SessionToken { fromSessionToken :: ST }
    deriving (Eq, Ord, Show, Read, Typeable, Generic, IsString, FromText)

instance Aeson.FromJSON SessionToken where parseJSON = Aeson.gparseJson
instance Aeson.ToJSON SessionToken where toJSON = Aeson.gtoJson

newtype TimeStamp = TimeStamp { fromTimeStamp :: UTCTime }
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

newtype Timeout = Timeout { fromTimeout :: NominalDiffTime }
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

timeStampToString :: TimeStamp -> String
timeStampToString = formatTime defaultTimeLocale "%FT%T%Q%z" . fromTimeStamp

timeStampFromString :: Monad m => String -> m TimeStamp
timeStampFromString raw = maybe (fail $ "TimeStamp: no parse: " ++ show raw) return $
  TimeStamp <$> parseTime defaultTimeLocale "%FT%T%Q%z" raw

instance SafeCopy TimeStamp
  where
    putCopy = contain . safePut . timeStampToString
    getCopy = contain $ safeGet >>= timeStampFromString

instance Aeson.FromJSON TimeStamp
  where
    parseJSON = (>>= timeStampFromString) . Aeson.parseJSON

instance Aeson.ToJSON TimeStamp
  where
    toJSON = Aeson.toJSON . timeStampToString

timeoutToString :: Timeout -> String
timeoutToString = show . (toSeconds :: NominalDiffTime -> Double) . fromTimeout

timeoutFromString :: Monad m => String -> m Timeout
timeoutFromString raw = maybe (fail $ "Timeout: no parse: " ++ show raw) return $
  Timeout . (fromSeconds :: Double -> NominalDiffTime) <$> readMay raw

instance SafeCopy Timeout
  where
    putCopy = contain . safePut . timeoutToString
    getCopy = contain $ safeGet >>= timeoutFromString

instance Aeson.FromJSON Timeout
  where
    parseJSON = (>>= timeoutFromString) . Aeson.parseJSON

instance Aeson.ToJSON Timeout
  where
    toJSON = Aeson.toJSON . timeoutToString


-- * role, agent, lio

-- | Some thing or body that deals with (and can authenticate itself
-- before) thentos.  Examples: 'User' or 'Service'.  (We could have
-- called this 'Principal', but that name is in use by LIO already.)
data Agent = UserA !UserId | ServiceA !ServiceId
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance Aeson.FromJSON Agent where parseJSON = Aeson.gparseJson
instance Aeson.ToJSON Agent where toJSON = Aeson.gtoJson

data Role =
    RoleAdmin
    -- ^ Can do anything.  (FIXME: do we even need a role for this, or
    -- could we just use 'True'?)

  | RoleOwnsUsers
    -- ^ Can do anything to map 'dbUsers'

  | RoleOwnsUnconfirmedUsers
    -- ^ Can do anything to map 'dbUnConfirmedUsers'

  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance Aeson.FromJSON Role where parseJSON = Aeson.gparseJson
instance Aeson.ToJSON Role where toJSON = Aeson.gtoJson

instance ToCNF Agent where toCNF = toCNF . show
instance ToCNF Role where toCNF = toCNF . show

-- | Wrapper for lio's 'Labeled' to avoid orphan instances.  (Also,
-- freeze 'ThentosLabel' as label type.)
data ThentosLabeled t =
    ThentosLabeled
      { thentosLabelL :: ThentosLabel
      , thentosLabelV :: t
      }
  deriving (Eq, Ord, Show, Read, Typeable, Functor)

instance (SafeCopy t, Show t, Read t) => SafeCopy (ThentosLabeled t)
  where
    putCopy = contain . safePut . show
    getCopy = contain $ safeGet >>= \ raw ->
      maybe (fail $ "instance SafeCopy ThentosLabeled: no parse" ++ show raw) return . readMay $ raw

-- | Wrapper for lio's 'DCLabel' to avoid orphan instances.
newtype ThentosLabel = ThentosLabel { fromThentosLabel :: DCLabel }
  deriving (Eq, Ord, Show, Read, Typeable)

instance SafeCopy ThentosLabel
  where
    putCopy = contain . safePut . show
    getCopy = contain $ safeGet >>= \ raw ->
      maybe (fail $ "instance SafeCopy ThentosLabel: no parse" ++ show raw) return . readMay $ raw

instance Label ThentosLabel where
    lub (ThentosLabel l) (ThentosLabel l') = ThentosLabel $ lub l l'
    glb (ThentosLabel l) (ThentosLabel l') = ThentosLabel $ glb l l'
    canFlowTo (ThentosLabel l) (ThentosLabel l') = canFlowTo l l'

newtype ThentosClearance = ThentosClearance { fromThentosClearance :: DCLabel }
    deriving (Eq, Ord, Show, Read, Typeable)

instance SafeCopy ThentosClearance
  where
    putCopy = contain . safePut . show
    getCopy = contain $ safeGet >>= \ raw ->
      maybe (fail $ "instance SafeCopy DbError: no parse" ++ show raw) return . readMay $ raw

instance Label ThentosClearance where
    lub (ThentosClearance l) (ThentosClearance l') = ThentosClearance $ lub l l'
    glb (ThentosClearance l) (ThentosClearance l') = ThentosClearance $ glb l l'
    canFlowTo (ThentosClearance l) (ThentosClearance l') = canFlowTo l l'


-- * errors

-- we used to have one long sum type with one constructor for each
-- error anywhere in the system.  this approach lacks certain
-- desirable properties:
--
--  - if we want to add errors, we have to change this type.  this may
--    be hard for people who use thentos as a library to write
--    someting else: they have to decide whether they want to leave
--    the error type unchanged or stop using any functions from
--    thentos that touch it.
--
--  - we can't say (in types): "this transaction / snap handler /
--    servant handler / ... throws either 'NoSuchUser' or
--    'BadCredentials', but no other exceptions".  (if we could, we
--    could trigger type errors in cases where a handler calls an
--    action that calls a transaction that calls another transaction,
--    and somewhere down the call tree an exception is added that the
--    handler has ruled out.)
--
-- the following is an experiment to overcome these deficiencies.

class (Eq e, Ord e, Show e, Read e, Typeable e) => ThentosError e

data NoSuchUser = NoSuchUser
  deriving (Eq, Ord, Show, Read, Typeable)

instance ThentosError NoSuchUser

data NoSuchPendingUserConfirmation = NoSuchPendingUserConfirmation
  deriving (Eq, Ord, Show, Read, Typeable)

instance ThentosError NoSuchPendingUserConfirmation

data MalformedConfirmationToken = MalformedConfirmationToken ST
  deriving (Eq, Ord, Show, Read, Typeable)

instance ThentosError MalformedConfirmationToken

data NoSuchService = NoSuchService
  deriving (Eq, Ord, Show, Read, Typeable)

instance ThentosError NoSuchService

data NoSuchSession = NoSuchSession
  deriving (Eq, Ord, Show, Read, Typeable)

instance ThentosError NoSuchSession

data OperationNotPossibleInServiceSession = OperationNotPossibleInServiceSession
  deriving (Eq, Ord, Show, Read, Typeable)

instance ThentosError OperationNotPossibleInServiceSession

data ServiceAlreadyExists = ServiceAlreadyExists
  deriving (Eq, Ord, Show, Read, Typeable)

instance ThentosError ServiceAlreadyExists

data UserEmailAlreadyExists = UserEmailAlreadyExists
  deriving (Eq, Ord, Show, Read, Typeable)

instance ThentosError UserEmailAlreadyExists

data UserNameAlreadyExists = UserNameAlreadyExists
  deriving (Eq, Ord, Show, Read, Typeable)

instance ThentosError UserNameAlreadyExists

data PermissionDenied = PermissionDenied String ThentosClearance ThentosLabel
  deriving (Eq, Ord, Show, Read, Typeable)

instance ThentosError PermissionDenied

data BadCredentials = BadCredentials
  deriving (Eq, Ord, Show, Read, Typeable)

instance ThentosError BadCredentials

data BadAuthenticationHeaders = BadAuthenticationHeaders
  deriving (Eq, Ord, Show, Read, Typeable)

instance ThentosError BadAuthenticationHeaders

data ProxyNotAvailable = ProxyNotAvailable
  deriving (Eq, Ord, Show, Read, Typeable)

instance ThentosError ProxyNotAvailable

data MissingServiceHeader = MissingServiceHeader
  deriving (Eq, Ord, Show, Read, Typeable)

instance ThentosError MissingServiceHeader

data ProxyNotConfiguredForService = ProxyNotConfiguredForService ServiceId
  deriving (Eq, Ord, Show, Read, Typeable)

instance ThentosError ProxyNotConfiguredForService

data NoSuchResetToken = NoSuchResetToken
  deriving (Eq, Ord, Show, Read, Typeable)

instance ThentosError NoSuchResetToken


-- | this instance is undecidable, overlapping, and orphan.  hm...
instance ThentosError e => SafeCopy e
  where
    putCopy = contain . safePut . show
    getCopy = contain $ safeGet >>= \ raw ->
      maybe (fail $ "instance SafeCopy ThentosError: no parse" ++ show raw) return . readMay $ raw


-- | Render errors for servant.  (The servant error type will
-- hopefully change in the future.)
class ThentosError e => ThentosErrorShowServant e where
    showThentosError :: MonadIO m => e -> m (Int, String)

instance ThentosErrorShowServant NoSuchUser where
    showThentosError NoSuchUser = return (404, "user not found")

instance ThentosErrorShowServant NoSuchPendingUserConfirmation where
    showThentosError NoSuchPendingUserConfirmation = return (404, "unconfirmed user not found")

instance ThentosErrorShowServant MalformedConfirmationToken where
    showThentosError (MalformedConfirmationToken path) = return (400, "malformed confirmation token: " ++ show path)

instance ThentosErrorShowServant NoSuchService where
    showThentosError NoSuchService = return (404, "service not found")

instance ThentosErrorShowServant NoSuchSession where
    showThentosError NoSuchSession = return (404, "session not found")

instance ThentosErrorShowServant OperationNotPossibleInServiceSession where
    showThentosError OperationNotPossibleInServiceSession = return (404, "operation not possible in service session")

instance ThentosErrorShowServant ServiceAlreadyExists where
    showThentosError ServiceAlreadyExists = return (403, "service already exists")

instance ThentosErrorShowServant UserEmailAlreadyExists where
    showThentosError UserEmailAlreadyExists = return (403, "email already in use")

instance ThentosErrorShowServant UserNameAlreadyExists where
    showThentosError UserNameAlreadyExists = return (403, "user name already in use")

instance ThentosErrorShowServant PermissionDenied where
    showThentosError e@(PermissionDenied _ _ _) = logger INFO (show e) >> return (401, "unauthorized")

instance ThentosErrorShowServant BadCredentials where
    showThentosError e@BadCredentials = logger INFO (show e) >> return (401, "unauthorized")

instance ThentosErrorShowServant BadAuthenticationHeaders where
    showThentosError BadAuthenticationHeaders = return (400, "bad authentication headers")

instance ThentosErrorShowServant ProxyNotAvailable where
    showThentosError ProxyNotAvailable = return (404, "proxying not activated")

instance ThentosErrorShowServant MissingServiceHeader where
    showThentosError MissingServiceHeader = return (404, "headers do not contain service id")

instance ThentosErrorShowServant ProxyNotConfiguredForService where
    showThentosError (ProxyNotConfiguredForService sid) = return (404, "proxy not configured for service " ++ show sid)

instance ThentosErrorShowServant NoSuchResetToken where
    showThentosError NoSuchResetToken = return (404, "no such password reset token")

-- for snap and log file, we can now implement similar classes.

-- now we can go anywhere and add error types, at the cost of a lot
-- more verbosity (there may be ways to reduce boilerplate again).


-- and we can restrict the error types that can be thrown by handlers
-- / actions / transactions.  the following is pseudo-code:

data LoginError =
    LoginError1 NoSuchUser
  | LoginError2 BadCredentials
  deriving ...

instance ThentosError LoginError

...

loginTrans :: UserName -> UserPass -> ThentosUpdate LoginError SessionToken

-- but this ain't so great.  now we need to re-instantiate
-- thentosErrorShowServant and all other classes for LoginError.  and
-- writing 'loginTrans' get's more awkward.


-- * boilerplate

makeLenses ''DB
makeLenses ''User
makeLenses ''Session
makeLenses ''Service

$(deriveSafeCopy 0 'base ''DB)
$(deriveSafeCopy 0 'base ''User)
$(deriveSafeCopy 0 'base ''Session)
$(deriveSafeCopy 0 'base ''Service)
$(deriveSafeCopy 0 'base ''ServiceId)
$(deriveSafeCopy 0 'base ''ServiceKey)
$(deriveSafeCopy 0 'base ''ServiceName)
$(deriveSafeCopy 0 'base ''ServiceDescription)
$(deriveSafeCopy 0 'base ''SessionToken)
$(deriveSafeCopy 0 'base ''UserEmail)
$(deriveSafeCopy 0 'base ''UserName)
$(deriveSafeCopy 0 'base ''ConfirmationToken)
$(deriveSafeCopy 0 'base ''PasswordResetToken)
$(deriveSafeCopy 0 'base ''Group)
$(deriveSafeCopy 0 'base ''UserId)
$(deriveSafeCopy 0 'base ''Agent)
$(deriveSafeCopy 0 'base ''Role)
