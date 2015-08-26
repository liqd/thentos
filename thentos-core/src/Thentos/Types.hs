{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveFunctor               #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE KindSignatures              #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE UndecidableInstances        #-}

module Thentos.Types where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Exception (Exception)
import Control.Monad (when, unless, mzero)
import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, ToJSON, Value(String))
import Data.Maybe (isNothing, fromMaybe)
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String.Conversions (SBS, ST, cs)
import Data.String (IsString)
import Data.Thyme.Time () -- required for NominalDiffTime's num instance
import Data.Thyme (UTCTime, NominalDiffTime, formatTime, parseTime, toSeconds, fromSeconds)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import LIO.DCLabel (ToCNF, toCNF)
import Safe (readMay)
import Servant.Common.Text (FromText)
import System.Locale (defaultTimeLocale)
import System.Random (Random)
import Text.Email.Validate (EmailAddress, emailAddress, toByteString)
import URI.ByteString (uriAuthority, uriQuery, uriScheme, schemeBS, uriFragment,
                       queryPairs, parseURI, laxURIParserOptions, authorityHost,
                       authorityPort, portNumber, hostBS, uriPath)
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)
import Database.PostgreSQL.Simple.FromField (FromField, fromField, ResultError(..), returnError)
import Database.PostgreSQL.Simple.ToField (ToField, toField)

import qualified Crypto.Scrypt as Scrypt
import qualified Data.Aeson as Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Generics.Generic.Aeson as Aeson


-- * user

-- | (user groups (the data that services want to store and retrieve in thentos) and session tokens
-- of all active sessions are stored in assoc lists rather than maps.  this saves us explicit json
-- instances for now.)
data User =
    User
      { _userName            :: !UserName
      , _userPassword        :: !(HashedSecret UserPass)
      , _userEmail           :: !UserEmail
      , _userThentosSessions :: !(Set ThentosSessionToken)
          -- ^ (service sessions are stored in the resp. value in @DB ^. dbSessions@)
      , _userServices        :: !(Map ServiceId ServiceAccount)
          -- ^ services (with session account information)
      }
  deriving (Eq, Show, Typeable, Generic)

instance FromRow User where
    fromRow = User <$> field <*> field <*> field <*> pure Set.empty <*> pure Map.empty

-- | the data a user maintains about a service they are signed up
-- with.
data ServiceAccount =
    ServiceAccount
      { _serviceAnonymous :: !Bool
        -- ^ Do not give out any information about user beyond session token validity bit.  (not
        -- implemented.)

        -- FUTURE WORK: what we actually would want here is "something" (type?  function?  something
        -- more creative?)  that can be used as a filter on 'User' and will hide things from the
        -- service as appropriate.  we also want 'Service' to contain a counterpart "something".
        -- and a matcher that takes a service "something" and a user "something" and computes a
        -- compromise (or 'Nothing' if there is a conflict).

      }
  deriving (Eq, Show, Typeable, Generic)

newServiceAccount :: ServiceAccount
newServiceAccount = ServiceAccount False

newtype UserId = UserId { fromUserId :: Integer }
    deriving (Eq, Ord, Enum, Show, Read, Random, FromJSON, ToJSON, Typeable, Generic, FromText)

instance ToField UserId where
    toField = toField . fromUserId

instance FromField UserId where
    fromField f dat = UserId <$> fromField f dat

newtype UserName = UserName { fromUserName :: ST }
    deriving (Eq, Ord, Show, Read, FromJSON, ToJSON, Typeable, Generic, IsString)

instance FromField UserName where
    fromField f dat = UserName <$> fromField f dat

instance ToField UserName where
    toField = toField . fromUserName

-- | FIXME: ToJSON instance should go away in order to avoid accidental leakage of cleartext
-- passwords.  but for the experimentation phase this is too much of a headache.  (Under no
-- circumstances render to something like "[password hidden]".  Causes a lot of confusion.)
newtype UserPass = UserPass { fromUserPass :: ST }
    deriving (Eq, FromJSON, ToJSON, Typeable, Generic, IsString)

newtype HashedSecret a = HashedSecret { fromHashedSecret :: Scrypt.EncryptedPass }
    deriving (Eq, Show, Typeable, Generic)

instance FromField (HashedSecret a) where
    fromField f dat = HashedSecret . Scrypt.EncryptedPass <$> fromField f dat

instance ToField (HashedSecret a) where
    toField = toField . Scrypt.getEncryptedPass . fromHashedSecret

newtype UserEmail = UserEmail { userEmailAddress :: EmailAddress }
    deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance FromField UserEmail where
    fromField f Nothing = returnError UnexpectedNull f ""
    fromField f (Just bs) = case parseUserEmail (cs bs) of
                              Nothing -> returnError ConversionFailed f ""
                              Just e  -> return e

instance ToField UserEmail where
    toField = toField . fromUserEmail

parseUserEmail :: ST -> Maybe UserEmail
parseUserEmail t = do
    email <- emailAddress (cs t)
    return $ UserEmail email

fromUserEmail :: UserEmail -> ST
fromUserEmail = cs . toByteString . userEmailAddress

instance Aeson.FromJSON UserEmail
  where
    parseJSON (String t) = case emailAddress $ cs t of
        Just email -> return $ UserEmail email
        Nothing    -> fail $ "Not a valid email address: " ++ cs t
    parseJSON bad        = fail $ "Not a valid email address: " ++ show bad

instance Aeson.ToJSON UserEmail
    where toJSON = Aeson.toJSON . fromUserEmail

newtype ConfirmationToken = ConfirmationToken { fromConfirmationToken :: ST }
    deriving (Eq, Ord, Show, Read, Typeable, Generic)

newtype PasswordResetToken = PasswordResetToken { fromPasswordResetToken :: ST }
    deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance ToField PasswordResetToken where
    toField = toField . fromPasswordResetToken

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

-- | (Service owner is an 'Agent', not a 'User', so that services can (but do not have to) be owned
-- by their parent services in a service hierarchy.)
data Service =
    Service
      { _serviceKey            :: !(HashedSecret ServiceKey)
      , _serviceOwner          :: !Agent
      , _serviceThentosSession :: !(Maybe ThentosSessionToken)
          -- ^ Used by the service to authenticate in communication with thentos.
      , _serviceName           :: !ServiceName
      , _serviceDescription    :: !ServiceDescription
      , _serviceGroups         :: !(Map GroupNode (Set Group))
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

newtype Group = Group { fromGroup :: ST }
    deriving (Eq, Ord, Show, Read, Typeable, Generic, IsString)

instance Aeson.FromJSON Group where parseJSON = Aeson.gparseJson
instance Aeson.ToJSON Group where toJSON = Aeson.gtoJson

data GroupNode =
        GroupG { fromGroupG :: Group }
      | GroupU { fromGroupU :: UserId }
    deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance Aeson.FromJSON GroupNode where parseJSON = Aeson.gparseJson
instance Aeson.ToJSON GroupNode where toJSON = Aeson.gtoJson


-- * thentos and service session

newtype ThentosSessionToken = ThentosSessionToken { fromThentosSessionToken :: ST }
    deriving (Eq, Ord, Show, Read, Typeable, Generic, IsString, FromText, FromJSON, ToJSON)

data ThentosSession =
    ThentosSession
      { _thSessAgent           :: !Agent
      , _thSessStart           :: !Timestamp
      , _thSessEnd             :: !Timestamp
      , _thSessExpirePeriod    :: !Timeout
      , _thSessServiceSessions :: !(Set ServiceSessionToken)
      }
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

newtype ServiceSessionToken = ServiceSessionToken { fromServiceSessionToken :: ST }
    deriving (Eq, Ord, Show, Read, Typeable, Generic, IsString, FromText)

instance Aeson.FromJSON ServiceSessionToken where parseJSON = Aeson.gparseJson
instance Aeson.ToJSON ServiceSessionToken where toJSON = Aeson.gtoJson

data ServiceSession =
    ServiceSession
      { _srvSessService        :: !ServiceId
      , _srvSessStart          :: !Timestamp
      , _srvSessEnd            :: !Timestamp
      , _srvSessExpirePeriod   :: !Timeout
      , _srvSessThentosSession :: !ThentosSessionToken
      , _srvSessMetadata       :: !ServiceSessionMetadata
      }
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance Aeson.FromJSON ServiceSession where parseJSON = Aeson.gparseJson
instance Aeson.ToJSON ServiceSession where toJSON = Aeson.gtoJson

data ServiceSessionMetadata =
    ServiceSessionMetadata
      { _srvSessMdUser :: !UserName
      }
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance Aeson.FromJSON ServiceSessionMetadata where parseJSON = Aeson.gparseJson
instance Aeson.ToJSON ServiceSessionMetadata where toJSON = Aeson.gtoJson


-- * timestamp, timeout

newtype Timestamp = Timestamp { fromTimestamp :: UTCTime }
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

newtype Timeout = Timeout { fromTimeout :: NominalDiffTime }
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance ToField Timeout where
    toField = toField . timeoutToString
    -- TODO: is this actually the right format?

timeStampToString :: Timestamp -> String
timeStampToString = formatTime defaultTimeLocale "%FT%T%Q%z" . fromTimestamp

timeStampFromString :: Monad m => String -> m Timestamp
timeStampFromString raw = maybe (fail $ "Timestamp: no parse: " ++ show raw) return $
  Timestamp <$> parseTime defaultTimeLocale "%FT%T%Q%z" raw

instance Aeson.FromJSON Timestamp
  where
    parseJSON = (>>= timeStampFromString) . Aeson.parseJSON

instance Aeson.ToJSON Timestamp
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


-- * role, agent, lio

-- | Some thing or body that deals with (and can authenticate itself before) thentos.  Examples:
-- 'User' or 'Service'.  (We could have called this 'Principal', but that name is in use by LIO
-- already.)
data Agent = UserA !UserId | ServiceA !ServiceId
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance Aeson.FromJSON Agent where parseJSON = Aeson.gparseJson
instance Aeson.ToJSON Agent where toJSON = Aeson.gtoJson

data RoleBasic =
    RoleAdmin
    -- ^ Can do anything.  (There may be no difference in behaviour from 'allowEverything'
    -- resp. 'thentosPublic', but if we ever want to restrict privileges, it's easier if it is a
    -- 'Role'.)

  | RoleOwnsUsers
    -- ^ Can do anything to map 'dbUsers' in 'DB'

  | RoleOwnsUnconfirmedUsers
    -- ^ Can do anything to map 'dbUnConfirmedUsers' in 'DB'

  | RoleUser
    -- ^ Can sign up with services

  | RoleUserAdmin
    -- ^ Can create (and manage her own) users

  | RoleServiceAdmin
    -- ^ Can create (and manage her own) services
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Generic)

instance Aeson.FromJSON RoleBasic where parseJSON = Aeson.gparseJson
instance Aeson.ToJSON RoleBasic where toJSON = Aeson.gtoJson

-- | Recursive role hierarchies.
data Role =
    Roles [Role]
  | RoleBasic RoleBasic
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance Aeson.FromJSON Role where parseJSON = Aeson.gparseJson
instance Aeson.ToJSON Role where toJSON = Aeson.gtoJson

instance ToCNF Agent where toCNF = toCNF . show
instance ToCNF RoleBasic where toCNF = toCNF . show
-- (No CNF instance for Role for now.  We unravel role hierarchies during label construction.)


-- * uri

data ProxyUri = ProxyUri { proxyHost :: SBS
                         , proxyPort :: Int
                         , proxyPath :: SBS
                         }
    deriving (Eq, Typeable, Generic)


instance Aeson.FromJSON ProxyUri
  where
    parseJSON (String t) = case parseURI laxURIParserOptions $ cs t of
        Right uri -> do
            when (schemeBS (uriScheme uri) /= "http") mzero
            unless (null . queryPairs $ uriQuery uri) mzero
            unless (isNothing $ uriFragment uri) mzero
            auth <- maybe mzero return $ uriAuthority uri
            let host = authorityHost auth
                port = fromMaybe 80 $ portNumber <$> authorityPort auth
            return ProxyUri { proxyHost = hostBS host
                            , proxyPort = port
                            , proxyPath = uriPath uri
                            }
        Left _ -> mzero
    parseJSON bad        = fail $ "Not a valid URI (expected string): " ++ show bad

instance Aeson.ToJSON ProxyUri where
    toJSON (ProxyUri host port path) = Aeson.String $ cs host
                                                   <> cs (show port)
                                                   <> cs path

instance Show ProxyUri where
    show (ProxyUri host port path) = "http://" ++ host' ++ port' ++ path'
        where
            path' = case cs path of
                a@('/' : _) -> a
                a           -> '/' : a
            port' = ':' : show port
            host' = case reverse (cs host) of
                ('/':xs) -> reverse xs
                _        -> cs host


-- * errors

data ThentosError =
      NoSuchUser
    | NoSuchPendingUserConfirmation
    | MalformedConfirmationToken ST
    | NoSuchService
    | NoSuchThentosSession
    | NoSuchServiceSession
    | OperationNotPossibleInServiceSession
    | ServiceAlreadyExists
    | NotRegisteredWithService
    | UserEmailAlreadyExists
    | UserNameAlreadyExists
    | UserIdAlreadyExists
    | BadCredentials
    | BadAuthenticationHeaders
    | ProxyNotAvailable
    | MissingServiceHeader
    | ProxyNotConfiguredForService ServiceId
    | NoSuchToken
    | NeedUserA ThentosSessionToken ServiceId
    | MalformedUserPath ST
    deriving (Eq, Read, Show, Typeable)


instance Exception ThentosError


-- * boilerplate

makeLenses ''Service
makeLenses ''ServiceAccount
makeLenses ''ServiceSession
makeLenses ''ThentosSession
makeLenses ''User
