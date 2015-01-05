{-# LANGUAGE DeriveDataTypeable                       #-}
{-# LANGUAGE DeriveGeneric                            #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TemplateHaskell                          #-}

{-# OPTIONS  #-}

module Types
where

import Control.Lens (makeLenses)
import Data.Data (Typeable)
import Data.Functor.Infix ((<$>))
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.SafeCopy (SafeCopy, deriveSafeCopy, base, contain, putCopy, getCopy, safePut, safeGet)
import Data.String.Conversions (SBS, ST)
import Data.Thyme.Internal.Micro (Micro)
import Data.Thyme (UTCTime, NominalDiffTime, formatTime, parseTime, toSeconds, fromSeconds)
import GHC.Generics (Generic)
import Safe (readMay)
import System.Locale (defaultTimeLocale)

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
      , _userSession  :: Maybe Session
      }
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

type UserId = Int
type UserName = ST
type UserPass = ST
type UserEmail = ST
type Group = ST

data Session =
    Session
      { _sessionUser    :: !UserId
      , _sessionService :: !ServiceId
      , _sessionStart   :: !TimeStamp
      , _sessionEnd     :: !TimeStamp
      }
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

type SessionToken = ST

data Service =
    Service
      { _serviceKey  :: !ServiceKey
      }
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

type ServiceId = ST
type ServiceKey = ST


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
