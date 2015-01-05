{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE DeriveDataTypeable                       #-}
{-# LANGUAGE DeriveGeneric                            #-}
{-# LANGUAGE FlexibleInstances                        #-} -- FIXME: :-(
{-# LANGUAGE GeneralizedNewtypeDeriving               #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TemplateHaskell                          #-}

{-# OPTIONS  #-}

module Types
where

import Control.Applicative (pure, (<*>))
import Control.Lens (makeLenses)
import Data.Data (Typeable)
import Data.Functor.Infix ((<$>))
import Data.Map (Map)
import Data.SafeCopy (SafeCopy, deriveSafeCopy, base, contain, putCopy, getCopy, safePut, safeGet)
import Data.String (IsString)
import Data.String.Conversions (SBS, ST)
import Data.Thyme (UTCTime, NominalDiffTime, formatTime, parseTime, toSeconds, fromSeconds)
import GHC.Generics (Generic)
import Safe (readMay)
import Servant.API (Capture)
import Servant.Common.Text (FromText)
import Servant.Docs (ToCapture(..), DocCapture(DocCapture))
import Servant.Docs (ToSample(toSample))
import System.Locale (defaultTimeLocale)

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Generics.Generic.Aeson as Aeson

data DB =
    DB
      { _dbUsers       :: Map UserId User
      , _dbServices    :: Map ServiceId Service
      , _dbSessions    :: Map SessionToken Session

      , _dbFreshUserId :: !UserId
      , _dbRandomness  :: !SBS
      }
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

data User =
    User
      { _userName     :: !UserName
      , _userPassword :: !UserPass
      , _userEmail    :: !UserEmail
      , _userGroups   :: [Group]
      -- FIXME: should be a list of session ids (or maybe should not exist)
      , _userSession  :: Maybe Session
      }
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

newtype UserId = UserId { fromUserId :: Int }
    deriving (Eq, Ord, Enum, FromJSON, ToJSON, Show, Read, Typeable, Generic, Bounded, FromText)

newtype UserName = Username { fromUserName :: ST }
    deriving (Eq, Ord, FromJSON, ToJSON, Show, Read, Typeable, Generic, IsString)

newtype UserPass = UserPass { fromUserPass :: ST }
    deriving (Eq, Ord, FromJSON, ToJSON, Show, Read, Typeable, Generic, IsString)

newtype UserEmail = UserEmail { fromUserEmail :: ST }
    deriving (Eq, Ord, FromJSON, ToJSON, Show, Read, Typeable, Generic, IsString)

newtype Group = Group { fromGroup :: ST }
    deriving (Eq, Ord, FromJSON, ToJSON, Show, Read, Typeable, Generic, IsString)

data Session =
    Session
      { _sessionUser    :: !UserId
      , _sessionService :: !ServiceId
      , _sessionStart   :: !TimeStamp
      , _sessionEnd     :: !TimeStamp
      }
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

newtype SessionToken = SessionToken { fromSessionToken :: ST }
    deriving (Eq, Ord, FromJSON, ToJSON, Show, Read, Typeable, Generic, IsString, FromText)

data Service =
    Service
      { _serviceKey  :: !ServiceKey
      }
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

newtype ServiceId = ServiceId { fromServiceId :: ST }
    deriving (Eq, Ord, FromJSON, ToJSON, Show, Read, Typeable, Generic, IsString, FromText)

newtype ServiceKey = ServiceKey { fromServiceKey :: ST }
    deriving (Eq, Ord, FromJSON, ToJSON, Show, Read, Typeable, Generic, IsString)

newtype TimeStamp = TimeStamp { fromTimeStamp :: UTCTime }
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

newtype Timeout = Timeout { fromTimeout :: NominalDiffTime }
  deriving (Eq, Ord, Show, Read, Typeable, Generic)


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
$(deriveSafeCopy 0 'base ''Group)
$(deriveSafeCopy 0 'base ''UserId)
$(deriveSafeCopy 0 'base ''UserPass)

instance Aeson.FromJSON User      where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON Session   where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON Service   where parseJSON = Aeson.gparseJson

instance Aeson.ToJSON User        where toJSON = Aeson.gtoJson
instance Aeson.ToJSON Session     where toJSON = Aeson.gtoJson
instance Aeson.ToJSON Service     where toJSON = Aeson.gtoJson


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

instance Aeson.FromJSON Timeout
  where
    parseJSON = (>>= timeoutFromString) . Aeson.parseJSON

instance Aeson.ToJSON Timeout
  where
    toJSON = Aeson.toJSON . timeoutToString

-- instances for generating docs

instance ToCapture (Capture "token" SessionToken) where
    toCapture _ = DocCapture "token" "Session Token"

instance ToCapture (Capture "sid" ServiceId) where
    toCapture _ = DocCapture "sid" "Service ID"

instance ToCapture (Capture "userid" UserId) where
    toCapture _ = DocCapture "userid" "User ID"

instance ToSample Service where
    toSample = Just $ Service "98761234foo"

instance ToSample Session where
    toSample =
        Session <$> toSample
                <*> toSample
                <*> pure (TimeStamp $ read "1986-20-09 00:00:00 UTC")
                <*> pure (TimeStamp $ read "1986-27-09 00:00:00 UTC")

instance ToSample SessionToken where
    toSample = Just "abde1234llkjh"

instance ToSample [SessionToken] where
    toSample = Just ["abde1234llkjh", "47202sdfsg"]

instance ToSample User where
    toSample = Just $ User (Username "Kurt Cobain")
                           (UserPass "Hunter2")
                           (UserEmail "cobain@nirvana.com")
                           []
                           Nothing

instance ToSample UserId where
    toSample = Just $ UserId 12

instance ToSample [UserId] where
    toSample = Just [UserId 3, UserId 7, UserId 23]

instance ToSample ServiceId where
    toSample = Just "23t92ege0n"

instance ToSample [ServiceId] where
    toSample = Just ["23t92ege0n", "f4ghwgegin0"]

instance ToSample (UserId, ServiceId, Timeout) where
    toSample = (,,) <$> toSample <*> toSample <*> pure (Timeout $ fromSeconds 123456.0)

instance ToSample (UserId, ServiceId) where
    toSample = (,) <$> toSample <*> toSample

instance ToSample () where
    toSample = Just ()

instance ToSample Bool where
    toSample = Just True
