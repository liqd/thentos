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
import LIO.DCLabel (DCLabel, ToCNF, toCNF)
import Safe (readMay)
import Servant.API (Capture)
import Servant.Common.Text (FromText)
import Servant.Docs (ToCapture(..), DocCapture(DocCapture), ToSample(toSample))
import System.Locale (defaultTimeLocale)

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Generics.Generic.Aeson as Aeson

data DB =
    DB
      { _dbUsers       :: Map UserId User
      , _dbServices    :: Map ServiceId Service
      , _dbSessions    :: Map SessionToken Session
      , _dbRoles       :: Map Agent [Role]

      , _dbFreshUserId :: !UserId
      , _dbRandomness  :: !SBS
      }
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

-- | (user groups (the data that services want to store and retrieve
-- in thentos) and session tokens of all active sessions are stored in
-- assoc lists rather than maps.  this saves us explicit json
-- instances for now.)
data User =
    User
      { _userName     :: !UserName
      , _userPassword :: !UserPass
      , _userEmail    :: !UserEmail
      , _userGroups   :: [(ServiceId, [Group])]
      , _userSessions :: [(ServiceId, SessionToken)]
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


-- | Some thing or body that deals with (and can authenticate itself
-- before) thentos.  Examples: 'User' or 'Service'.  (We could have
-- called this 'Principal', but that name is in use by LIO already.)
data Agent = UserA User | ServiceA Service
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

data Role = RoleAdmin | RoleUser | RoleService
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

thentosLabeled :: DCLabel -> t -> ThentosLabeled t
thentosLabeled label = ThentosLabeled (ThentosLabel label)

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
$(deriveSafeCopy 0 'base ''Agent)
$(deriveSafeCopy 0 'base ''Role)


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
-- FIXME: move to a separate module and add a module-specific
-- -fno-warn-orphan-instances (since they are clearly harmless).

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
                           []

instance ToSample UserId where
    toSample = Just $ UserId 12

instance ToSample [UserId] where
    toSample = Just [UserId 3, UserId 7, UserId 23]

instance ToSample ServiceId where
    toSample = Just "23t92ege0n"

instance ToSample [ServiceId] where
    toSample = Just ["23t92ege0n", "f4ghwgegin0"]

instance ToSample (UserId, ServiceId, Timeout) where
    toSample = (,,) <$> toSample <*> toSample <*> pure (Timeout $ fromSeconds (123456.0 :: Double))

instance ToSample (UserId, ServiceId) where
    toSample = (,) <$> toSample <*> toSample

instance ToSample () where
    toSample = Just ()

instance ToSample Bool where
    toSample = Just True

instance ToSample (UserId, User) where
    toSample = Nothing

instance ToSample (ServiceId, Service) where
    toSample = Nothing

instance ToSample (SessionToken, Session) where
    toSample = Nothing
