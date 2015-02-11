{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE DeriveDataTypeable                       #-}
{-# LANGUAGE DeriveFunctor                            #-}
{-# LANGUAGE DeriveGeneric                            #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving               #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TemplateHaskell                          #-}

{-# OPTIONS  #-}

module Types where

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Typeable)
import Data.Functor.Infix ((<$>))
import Data.Map (Map)
import Data.SafeCopy (SafeCopy, deriveSafeCopy, base, contain, putCopy, getCopy, safePut, safeGet)
import Data.String.Conversions (ST)
import Data.String (IsString(fromString))
import Data.Text.Encoding (encodeUtf8)
import Data.Thyme (UTCTime, NominalDiffTime, formatTime, parseTime, toSeconds, fromSeconds)
import GHC.Generics (Generic)
import LIO.DCLabel (DCLabel, ToCNF, toCNF)
import Safe (readMay)
import Servant.Common.Text (FromText)
import System.Locale (defaultTimeLocale)

import qualified Crypto.Scrypt as Scrypt
import qualified Data.Aeson as Aeson
import qualified Generics.Generic.Aeson as Aeson


-- * db

data DB =
    DB
      { _dbUsers            :: Map UserId User
      , _dbUnconfirmedUsers :: Map ConfirmationToken User
      , _dbServices         :: Map ServiceId Service
      , _dbSessions         :: Map SessionToken Session
      , _dbRoles            :: Map Agent [Role]
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
      , _userPassword :: !EncryptedPass
      , _userEmail    :: !UserEmail
      , _userGroups   :: [(ServiceId, [Group])]
      , _userSession  :: !(Maybe SessionToken)
      , _userLogins   :: [ServiceId]
      }
  deriving (Eq, Show, Typeable, Generic)

newtype UserId = UserId { fromUserId :: Integer }
    deriving (Eq, Ord, Enum, FromJSON, ToJSON, Show, Read, Typeable, Generic, FromText)

newtype UserName = UserName { fromUserName :: ST }
    deriving (Eq, Ord, FromJSON, ToJSON, Show, Read, Typeable, Generic, IsString)

newtype UserPass = UserPass { fromUserPass :: Scrypt.Pass }
    deriving (Eq, Show, Typeable, Generic)

instance FromJSON UserPass where
    parseJSON = Aeson.withText "user password string" $ return . UserPass . Scrypt.Pass . encodeUtf8

instance ToJSON UserPass where
    toJSON _ = "[password hidden]"

instance IsString UserPass where
    fromString = UserPass . Scrypt.Pass . fromString

newtype EncryptedPass = EncryptedPass { fromEncryptedPass :: Scrypt.EncryptedPass }
    deriving (Eq, Show, Typeable, Generic)

instance SafeCopy EncryptedPass where
    putCopy = contain . safePut . Scrypt.getEncryptedPass . fromEncryptedPass
    getCopy = contain $ safeGet >>= return . EncryptedPass . Scrypt.EncryptedPass

newtype UserEmail = UserEmail { fromUserEmail :: ST }
    deriving (Eq, Ord, FromJSON, ToJSON, Show, Read, Typeable, Generic, IsString)

newtype Group = Group { fromGroup :: ST }
    deriving (Eq, Ord, FromJSON, ToJSON, Show, Read, Typeable, Generic, IsString)

newtype ConfirmationToken = ConfirmationToken { fromConfimationToken :: ST }
    deriving (Eq, Ord, FromJSON, ToJSON, Show, Read, Typeable, Generic)

-- | Information required to create a new User
data UserFormData =
    UserFormData
        { udName     :: !UserName
        , udPassword :: !UserPass
        , udEmail    :: !UserEmail
        }
    deriving (Eq, Show, Typeable, Generic)

instance Aeson.FromJSON UserFormData where parseJSON = Aeson.gparseJson
instance Aeson.ToJSON UserFormData where toJSON = Aeson.gtoJson


-- * service

data Service =
    Service
      { _serviceKey     :: !ServiceKey
      , _serviceSession :: !(Maybe SessionToken)
      }
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance Aeson.FromJSON Service where parseJSON = Aeson.gparseJson
instance Aeson.ToJSON Service where toJSON = Aeson.gtoJson

newtype ServiceId = ServiceId { fromServiceId :: ST }
  deriving (Eq, Ord, FromJSON, ToJSON, Show, Read, Typeable, Generic, IsString, FromText)

newtype ServiceKey = ServiceKey { fromServiceKey :: ST }
  deriving (Eq, Ord, FromJSON, ToJSON, Show, Read, Typeable, Generic, IsString)


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
    deriving (Eq, Ord, FromJSON, ToJSON, Show, Read, Typeable, Generic, IsString, FromText)

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

data Role = RoleAdmin
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

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

newtype ThentosClearance = ThentosClearance { fromThentosClearance :: DCLabel }
    deriving (Eq, Ord, Show, Read)

instance SafeCopy ThentosClearance
  where
    putCopy = contain . safePut . show
    getCopy = contain $ safeGet >>= \ raw ->
      maybe (fail $ "instance SafeCopy DbError: no parse" ++ show raw) return . readMay $ raw


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
$(deriveSafeCopy 0 'base ''SessionToken)
$(deriveSafeCopy 0 'base ''UserEmail)
$(deriveSafeCopy 0 'base ''UserName)
$(deriveSafeCopy 0 'base ''ConfirmationToken)
$(deriveSafeCopy 0 'base ''Group)
$(deriveSafeCopy 0 'base ''UserId)
$(deriveSafeCopy 0 'base ''Agent)
$(deriveSafeCopy 0 'base ''Role)
