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
{-# LANGUAGE ViewPatterns                #-}
{-# LANGUAGE UndecidableInstances        #-}

module Thentos.Types where

import Control.Exception (Exception)
import Control.Monad (when, unless, mzero)
import Control.Monad.Except (MonadError, throwError)
import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, ToJSON, Value(String), (.=))
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.ByteString.Builder (doubleDec)
import Data.Char (isAlpha)
import Data.Maybe (isNothing, fromMaybe)
import Data.Monoid ((<>))
import Data.String.Conversions (ConvertibleStrings, SBS, ST, cs)
import Data.String (IsString)
import Data.Thyme.Time (fromThyme, toThyme)
import Data.Thyme (UTCTime, formatTime, parseTime)
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
import Database.PostgreSQL.Simple.Missing (intervalSeconds)
import Database.PostgreSQL.Simple.FromField (FromField, fromField, ResultError(..), returnError,
                                             typeOid)
import Database.PostgreSQL.Simple.ToField (Action(Plain), ToField, inQuotes, toField)
import Database.PostgreSQL.Simple.TypeInfo (typoid)
import Database.PostgreSQL.Simple.TypeInfo.Static (interval)

import qualified Crypto.Scrypt as Scrypt
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as H
import qualified Data.Text as ST
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
      }
  deriving (Eq, Show, Typeable, Generic)

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
    deriving (Eq, Ord, Enum, Show, Read, Random, FromJSON, ToJSON, Typeable, Generic, FromText,
              FromField, ToField)

newtype UserName = UserName { fromUserName :: ST }
    deriving (Eq, Ord, Show, Read, FromJSON, ToJSON, Typeable, Generic, IsString, FromField,
              ToField)

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
    deriving (Eq, Ord, Show, Read, Typeable, Generic, ToField, FromField, IsString)

newtype PasswordResetToken = PasswordResetToken { fromPasswordResetToken :: ST }
    deriving (Eq, Ord, Show, Read, Typeable, Generic, IsString, FromField, ToField)

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
      }
  deriving (Eq, Show, Typeable, Generic)

newtype ServiceId = ServiceId { fromServiceId :: ST }
  deriving (Eq, Ord, Show, Read, Typeable, Generic, IsString, FromText, FromField, ToField)

instance Aeson.FromJSON ServiceId where parseJSON = Aeson.gparseJson
instance Aeson.ToJSON ServiceId where toJSON = Aeson.gtoJson

newtype ServiceKey = ServiceKey { fromServiceKey :: ST }
  deriving (Eq, Ord, Show, Read, Typeable, Generic, IsString)

instance Aeson.FromJSON ServiceKey where parseJSON = Aeson.gparseJson
instance Aeson.ToJSON ServiceKey where toJSON = Aeson.gtoJson

newtype ServiceName = ServiceName { fromServiceName :: ST }
  deriving (Eq, Ord, Show, Read, Typeable, Generic, IsString, FromText, FromField, ToField)

instance Aeson.FromJSON ServiceName where parseJSON = Aeson.gparseJson
instance Aeson.ToJSON ServiceName where toJSON = Aeson.gtoJson

newtype ServiceDescription = ServiceDescription { fromServiceDescription :: ST }
  deriving (Eq, Ord, Show, Read, Typeable, Generic, IsString, FromText, FromField, ToField)

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


-- * persona and context

newtype PersonaId = PersonaId { fromPersonaId :: Integer }
    deriving (Eq, Ord, Enum, Show, Read, Random, FromJSON, ToJSON, Typeable, Generic, FromText,
              FromField, ToField)

data Persona = Persona
  { _personaId   :: PersonaId
  , _personaName :: ST
  , _personaUid  :: UserId
  } deriving (Eq, Show, Typeable, Generic)

newtype ContextId = ContextId { fromContextId :: Integer }
    deriving (Eq, Ord, Enum, Show, Read, Random, FromJSON, ToJSON, Typeable, Generic, FromText,
              FromField, ToField)

newtype ContextName = ContextName { fromContextName :: ST }
    deriving (Eq, Ord, Show, Read, FromJSON, ToJSON, Typeable, Generic, IsString, FromField,
              ToField)

newtype ContextDescription = ContextDescription { fromContextDescription :: ST }
    deriving (Eq, Ord, Show, Read, FromJSON, ToJSON, Typeable, Generic, IsString, FromField,
              ToField)

data Context = Context
  { _contextId           :: ContextId
  , _contextOwnerService :: ServiceId
  , _contextName         :: ContextName
  , _contextDescription  :: ContextDescription
  , _contextUrl          :: ProxyUri
  } deriving (Eq, Show, Typeable, Generic)

-- * thentos and service session

newtype ThentosSessionToken = ThentosSessionToken { fromThentosSessionToken :: ST }
    deriving (Eq, Ord, Show, Read, Typeable, Generic, IsString, FromText, FromJSON, ToJSON, FromField, ToField)

data ThentosSession =
    ThentosSession
      { _thSessAgent           :: !Agent
      , _thSessStart           :: !Timestamp
      , _thSessEnd             :: !Timestamp
      , _thSessExpirePeriod    :: !Timeout
      }
  deriving (Eq, Ord, Show, Typeable, Generic)

newtype ServiceSessionToken = ServiceSessionToken { fromServiceSessionToken :: ST }
    deriving (Eq, Ord, Show, Read, Typeable, Generic, IsString, FromText, FromField, ToField)

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
  deriving (Eq, Ord, Show, Typeable, Generic)

instance Aeson.FromJSON ServiceSession where parseJSON = Aeson.gparseJson
instance Aeson.ToJSON ServiceSession where toJSON = Aeson.gtoJson

data ServiceSessionMetadata =
    ServiceSessionMetadata
      { _srvSessMdUser :: !UserName
      }
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance Aeson.FromJSON ServiceSessionMetadata where parseJSON = Aeson.gparseJson
instance Aeson.ToJSON ServiceSessionMetadata where toJSON = Aeson.gtoJson

instance FromField ServiceSessionMetadata where
    fromField f dat = ServiceSessionMetadata <$> fromField f dat

data ByUserOrServiceId = ByUser (UserId, UserPass)
                       | ByService (ServiceId, ServiceKey)
  deriving (Eq, Typeable, Generic)

instance FromJSON ByUserOrServiceId where
    parseJSON (Aeson.Object (H.toList -> [(key, val)]))
        | key == "user"    = ByUser <$> Aeson.parseJSON val
        | key == "service" = ByService <$> Aeson.parseJSON val
    parseJSON _ = mzero

instance ToJSON ByUserOrServiceId where
    toJSON (ByUser v)    = Aeson.object [ "user" .= Aeson.toJSON v]
    toJSON (ByService v) = Aeson.object [ "service" .= Aeson.toJSON v]


-- * timestamp, timeout

newtype Timestamp = Timestamp { fromTimestamp :: UTCTime }
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance ToField Timestamp where
    toField = toField . fromThyme . fromTimestamp

instance FromField Timestamp where
    fromField f dat = Timestamp . toThyme <$> fromField f dat

newtype Timeout = Timeoutms { toMilliseconds :: Int }
  deriving (Eq, Ord, Show)

toSeconds :: (Fractional a, Real a) => Timeout -> a
toSeconds = (/1000.0) . fromIntegral . toMilliseconds

fromMilliseconds :: Int -> Timeout
fromMilliseconds = Timeoutms

fromSeconds :: Int -> Timeout
fromSeconds = fromMilliseconds . (*1000)

fromMinutes :: Int -> Timeout
fromMinutes = fromSeconds . (*60)

fromHours :: Int -> Timeout
fromHours = fromMinutes . (*60)

fromDays :: Int -> Timeout
fromDays = fromHours . (*24)

instance ToField Timeout where
    toField = Plain . inQuotes . (<> " seconds") . doubleDec . toSeconds

instance FromField Timeout where
    fromField f mdat =
        if typeOid f /= typoid interval
            then returnError Incompatible f ""
            else case mdat of
                Nothing  -> returnError UnexpectedNull f ""
                Just dat -> case parseOnly intervalSeconds dat of
                    Left msg  -> returnError ConversionFailed f msg
                    Right t   -> return . Timeoutms . round . (*1000) $ t

timestampToString :: Timestamp -> String
timestampToString = formatTime defaultTimeLocale "%FT%T%Q%z" . fromTimestamp

timestampFromString :: Monad m => String -> m Timestamp
timestampFromString raw = maybe (fail $ "Timestamp: no parse: " ++ show raw) return $
  Timestamp <$> parseTime defaultTimeLocale "%FT%T%Q%z" raw

instance Aeson.FromJSON Timestamp
  where
    parseJSON = (>>= timestampFromString) . Aeson.parseJSON

instance Aeson.ToJSON Timestamp
  where
    toJSON = Aeson.toJSON . timestampToString

timeoutToString :: Timeout -> String
timeoutToString = secondsToString . (toSeconds :: Timeout -> Double)

timeoutFromString :: Monad m => String -> m Timeout
timeoutFromString raw = Timeoutms . round . (*(1000::Double)) <$> secondsFromString raw

secondsToString :: (Show a, Fractional a, Real a) => a -> String
secondsToString s = show s ++ "s"

secondsFromString :: (Read a, Fractional a, Real a, Monad m) => String -> m a
secondsFromString raw = do
    let (ns, suff) = break isAlpha raw
    n <- maybe (fail $ "interval: no parse: number: " ++ ns) return $ readMay ns
    mult <- case suff of
        "ms" -> return 0.001
        "s"  -> return 1
        "m"  -> return 60
        "h"  -> return $ 60*60
        "d"  -> return $ 24*60*60
        _    -> fail $ "interval: no parse: unit: " ++ suff
    return $ mult * n

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

data Role =
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

instance Aeson.FromJSON Role where parseJSON = Aeson.gparseJson
instance Aeson.ToJSON Role where toJSON = Aeson.gtoJson

instance ToCNF Agent where toCNF = toCNF . show
instance ToCNF Role where toCNF = toCNF . show

instance ToField Role where
    toField = toField . show

instance FromField Role where
    fromField f dat = do
        s <- fromField f dat
        case readMay s of
            Just r  -> return r
            Nothing -> returnError ConversionFailed f ""


-- * uri

data ProxyUri = ProxyUri { proxyHost :: SBS
                         , proxyPort :: Int
                         , proxyPath :: SBS
                         }
    deriving (Eq, Typeable, Generic)

renderProxyUri :: ProxyUri -> ST
renderProxyUri (ProxyUri host port path) = "http://" <> host' <> port' <//> cs path
  where
    host' :: ST = stripTrailingSlash $ cs host
    port' :: ST = if port == 80 then "" else cs $ ':' : show port

parseProxyUri :: MonadError String m => ST -> m ProxyUri
parseProxyUri t = case parseURI laxURIParserOptions $ cs t of
    Right uri -> do
        when (schemeBS (uriScheme uri) /= "http") $ throwError "Expected http schema"
        unless (null . queryPairs $ uriQuery uri) $ throwError "No query part allowed"
        unless (isNothing $ uriFragment uri) $ throwError "No URI fragment allowed"
        auth <- maybe (fail "No URI authority allowed") return $ uriAuthority uri
        let host = authorityHost auth
            port = fromMaybe 80 $ portNumber <$> authorityPort auth
        return ProxyUri { proxyHost = hostBS host
                        , proxyPort = port
                        , proxyPath = uriPath uri
                        }
    Left err -> throwError $ "Invalid URI: " ++ show err

instance Aeson.FromJSON ProxyUri
  where
    parseJSON (String t) = either fail return $ parseProxyUri t
    parseJSON bad        = fail $ "Not a valid URI (expected string): " ++ show bad

instance Aeson.ToJSON ProxyUri where
    toJSON = Aeson.String . renderProxyUri

instance Show ProxyUri where
    show = cs . renderProxyUri

instance FromField ProxyUri where
    fromField f Nothing = returnError UnexpectedNull f ""
    fromField f (Just bs) = case parseProxyUri $ cs bs of
        Left err  -> returnError ConversionFailed f err
        Right uri -> return uri

instance ToField ProxyUri where
    toField = toField . renderProxyUri

-- | Strip an optional slash from the start of a text. If the text doesn't start with a slash,
-- it is returned unchanged.
stripLeadingSlash :: ST -> ST
stripLeadingSlash p = if "/" `ST.isPrefixOf` p then ST.tail p else p

-- | Strip an optional slash from the end of a text. If the text doesn't end in a slash,
-- it is returned unchanged.
stripTrailingSlash :: ST -> ST
stripTrailingSlash p = if "/" `ST.isSuffixOf` p then ST.init p else p

-- | Path concatenation for avoiding double slashes in paths.  One
-- optional '/' trailing left side / leading right side is removed,
-- and one '/' is inserted.
(<//>) :: (ConvertibleStrings s ST, ConvertibleStrings ST s) => s -> s -> s
(cs -> p) <//> (cs -> p') = cs $ q <> "/" <> q'
  where
    q  :: ST = stripTrailingSlash p
    q' :: ST = stripLeadingSlash p'


-- * errors

data ThentosError e =
      NoSuchUser
    | NoSuchPendingUserConfirmation
    | MalformedConfirmationToken ST
    | ConfirmationTokenAlreadyExists
    | NoSuchService
    | NoSuchThentosSession
    | NoSuchServiceSession
    | NoSuchPersona
    | NoSuchContext
    | MultiplePersonasPerContext
    | OperationNotPossibleInServiceSession
    | ServiceAlreadyExists
    | NotRegisteredWithService
    | UserEmailAlreadyExists
    | UserNameAlreadyExists
    | UserIdAlreadyExists
    | PersonaNameAlreadyExists
    | ContextNameAlreadyExists
    | BadCredentials
    | BadAuthenticationHeaders
    | ProxyNotAvailable
    | MissingServiceHeader
    | ProxyNotConfiguredForService ServiceId
    | NoSuchToken
    | NeedUserA ThentosSessionToken ServiceId
    | MalformedUserPath ST
    | OtherError e
    deriving (Eq, Read, Show, Typeable)


instance (Show e, Typeable e) => Exception (ThentosError e)


-- * boilerplate

makeLenses ''Persona
makeLenses ''Context
makeLenses ''Service
makeLenses ''ServiceAccount
makeLenses ''ServiceSession
makeLenses ''ThentosSession
makeLenses ''User
