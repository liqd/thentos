{-# LANGUAGE DeriveDataTypeable                       #-}
{-# LANGUAGE DeriveGeneric                            #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TemplateHaskell                          #-}

{-# OPTIONS  #-}

module Types
where

import Control.Lens (makeLenses)
import Data.Data (Typeable)
import Data.Map (Map)
import Data.SafeCopy (deriveSafeCopy, base)
import Data.String.Conversions (SBS, ST)
import Data.Thyme.Internal.Micro (Micro)
import Data.Thyme (UTCTime, NominalDiffTime)
import GHC.Generics (Generic)

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
      , _sessionStart   :: !UTCTime  -- FIXME: UTCTime does not json-encode well.  we need to newtype this!
      , _sessionEnd     :: !UTCTime
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

makeLenses ''DB
makeLenses ''User
makeLenses ''Session
makeLenses ''Service

-- [FIXME: orphans!

$(deriveSafeCopy 0 'base ''UTCTime)
$(deriveSafeCopy 0 'base ''NominalDiffTime)
$(deriveSafeCopy 0 'base ''Micro)

instance Aeson.FromJSON UTCTime          where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON NominalDiffTime  where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON Micro            where parseJSON = Aeson.gparseJson

instance Aeson.ToJSON UTCTime            where toJSON = Aeson.gtoJson
instance Aeson.ToJSON NominalDiffTime    where toJSON = Aeson.gtoJson
instance Aeson.ToJSON Micro              where toJSON = Aeson.gtoJson

-- END FIXME]

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


newtype Timeout = Timeout NominalDiffTime
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance Aeson.FromJSON Timeout where parseJSON = Aeson.gparseJson
instance Aeson.ToJSON Timeout   where toJSON = Aeson.gtoJson
