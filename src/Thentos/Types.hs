{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE DeriveDataTypeable                       #-}
{-# LANGUAGE DeriveFunctor                            #-}
{-# LANGUAGE DeriveGeneric                            #-}
{-# LANGUAGE ExistentialQuantification                #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving               #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE StandaloneDeriving                       #-}
{-# LANGUAGE TemplateHaskell                          #-}

module Thentos.Types where

import Control.Exception (Exception)
import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Typeable, Proxy(Proxy), typeOf)
import Data.Dynamic (cast)
import Data.Functor.Infix ((<$>))
import Data.Map (Map)
import Data.SafeCopy (SafeCopy, Contained, deriveSafeCopy, base, contain, putCopy, getCopy, safePut, safeGet)
import Data.String.Conversions (ST)
import Data.String (IsString)
import Data.Thyme (UTCTime, NominalDiffTime, formatTime, parseTime, toSeconds, fromSeconds)
import GHC.Generics (Generic)
import LIO.DCLabel (DCLabel, ToCNF, toCNF)
import LIO.Label (Label, canFlowTo, glb, lub)
import Safe (readMay)
import Servant.Common.Text (FromText)
import System.Locale (defaultTimeLocale)

import qualified Crypto.Scrypt as Scrypt
import qualified Data.Aeson as Aeson
import qualified Data.Serialize as Cereal
import qualified Generics.Generic.Aeson as Aeson


-- * aux

putCopyViaShowRead :: (Show a) => a -> Contained Cereal.Put
putCopyViaShowRead = contain . safePut . show

getCopyViaShowRead :: forall a . (Typeable a, Read a) => Contained (Cereal.Get a)
getCopyViaShowRead = contain $ safeGet >>= \ raw -> maybe (_fail raw) return . readMay $ raw
  where
    _fail raw = fail $ "getCopyViaShowRead: no parse for " ++ show (raw, typeOf (Proxy :: Proxy a))


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
    getCopy = contain $ HashedSecret . Scrypt.EncryptedPass <$> safeGet

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

instance (Typeable t, Show t, Read t) => SafeCopy (ThentosLabeled t)
  where putCopy = putCopyViaShowRead; getCopy = getCopyViaShowRead

-- | Wrapper for lio's 'DCLabel' to avoid orphan instances.
newtype ThentosLabel = ThentosLabel { fromThentosLabel :: DCLabel }
  deriving (Eq, Ord, Show, Read, Typeable)

instance SafeCopy ThentosLabel
  where putCopy = putCopyViaShowRead; getCopy = getCopyViaShowRead

instance Label ThentosLabel where
    lub (ThentosLabel l) (ThentosLabel l') = ThentosLabel $ lub l l'
    glb (ThentosLabel l) (ThentosLabel l') = ThentosLabel $ glb l l'
    canFlowTo (ThentosLabel l) (ThentosLabel l') = canFlowTo l l'

newtype ThentosClearance = ThentosClearance { fromThentosClearance :: DCLabel }
    deriving (Eq, Ord, Show, Read, Typeable)

instance SafeCopy ThentosClearance
  where putCopy = putCopyViaShowRead; getCopy = getCopyViaShowRead

instance Label ThentosClearance where
    lub (ThentosClearance l) (ThentosClearance l') = ThentosClearance $ lub l l'
    glb (ThentosClearance l) (ThentosClearance l') = ThentosClearance $ glb l l'
    canFlowTo (ThentosClearance l) (ThentosClearance l') = canFlowTo l l'


-- * errors

class (Exception e, Typeable e, Show e, SafeCopy e) => ThentosError e where
    toThentosError :: e -> SomeThentosError
    toThentosError = SomeThentosError

    fromThentosError :: SomeThentosError -> Maybe e
    fromThentosError (SomeThentosError e) = cast e


data SomeThentosError = forall e . ThentosError e => SomeThentosError e
  deriving (Typeable)

instance Exception SomeThentosError

instance ThentosError SomeThentosError where
    toThentosError = id
    fromThentosError = Just

instance Show SomeThentosError where
    showsPrec p (SomeThentosError e) = showsPrec p e

-- | This instance is provided just to make acid-state happy.  It
-- always writes an 'UnknownThentosError' to disk, and reads back
-- nothing else, with the (acceptable) consequence that change log
-- re-runs give you less error information than the initial
-- transactions.  (For an alternative solution that actually does
-- store all existentially quantified information, see:
-- http://stackoverflow.com/questions/8101067/binary-instance-for-an-existential)
instance SafeCopy SomeThentosError
  where
    putCopy (SomeThentosError _) = contain $ safePut UnknownThentosError
    getCopy = contain $ SomeThentosError <$> (safeGet :: Cereal.Get UnknownThentosError)


-- | We use this for making 'SomeThentosError' serializable.
data UnknownThentosError = UnknownThentosError
  deriving (Eq, Ord, Show, Read, Typeable)

instance Exception UnknownThentosError
instance ThentosError UnknownThentosError
$(deriveSafeCopy 0 'base ''UnknownThentosError)


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
