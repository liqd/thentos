{-# LANGUAGE ConstraintKinds             #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Thentos.Types
    ( JsonTop(..)
    , User(..)
    , ServiceAccount(..), newServiceAccount
    , UserId(..)
    , UserName(..)
    , UserPass(..)
    , minPasswordLength
    , HashedSecret(..)
    , UserEmail(..), parseUserEmail, fromUserEmail
    , WrappedEmail(..)
    , ConfirmationToken(..)
    , PasswordResetToken(..)
    , PasswordResetRequest(..)
    , UserFormData(..)
    , UserCreationRequest(..)
    , LoginFormData(..)

    , Service(..)
    , ServiceId(..)
    , ServiceKey(..)
    , ServiceName(..)
    , ServiceDescription(..)
    , ServiceGroup(..)

    , PersonaId(..)
    , PersonaName(..)
    , Persona(..)
    , ContextId(..)
    , ContextName(..)
    , ContextDescription(..)
    , Context(..)

    , ThentosSessionToken(..)
    , GetThentosSessionToken(..)
    , MonadUseThentosSessionToken
    , ThentosSession(..)
    , ServiceSessionToken(..)
    , ServiceSession(..)
    , ServiceSessionMetadata(..)
    , ByUserOrServiceId(..)

    , EmailRecipients(..), erEmails, erPersonas
    , SendEmailRequest(..), emailRecipients, emailSubject, emailBody, emailHtml

    , Timestamp(..)
    , Timeout(..), toSeconds
    , fromMilliseconds, fromSeconds, fromMinutes, fromHours, fromDays
    , timestampToString, timestampFromString
    , timeoutToString, timeoutFromString
    , secondsToString, secondsFromString

    , Agent(..)
    , Group(..)

    , RelRef(..)
    , Uri(..), parseUri, renderUri
    , ProxyUri(..), renderProxyUri, parseProxyUri
    , (<//>), stripLeadingSlash, stripTrailingSlash

    , Random20, mkRandom20, fromRandom20
    , ImageData(..)
    , CaptchaId(..)
    , CaptchaSolution(..)
    , CaptchaAttempt(..)
    , SignupAttempt(..)

    , ThentosError(..)
    , MonadThentosError
    , MonadThentosIO

    -- Re-exported from Servant.Missing
    , MonadError500
    , ThrowError500(..)
    , Error500

    , personaId, personaName, personaUid, personaExternalUrl
    , contextDescription, contextId, contextName, contextService, contextUrl

    , serviceDescription, serviceKey, serviceName, serviceOwner
    , serviceThentosSession, serviceAnonymous

    , srvSessEnd, srvSessExpirePeriod, srvSessMetadata, srvSessService
    , srvSessStart, srvSessThentosSession

    , thSessAgent, thSessEnd, thSessExpirePeriod, thSessStart
    , userEmail, userName, userPassword

    , aesonError
    )
where

import Data.Aeson (FromJSON, ToJSON, Value(String), (.=), (.:))
import Data.Aeson.Types (Parser)
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Database.PostgreSQL.Simple.FromField
    (FromField, fromField, Conversion, ResultError(..), returnError, typeOid)
import Database.PostgreSQL.Simple.Missing (intervalSeconds)
import Database.PostgreSQL.Simple.ToField (Action(Plain), ToField, inQuotes, toField)
import Database.PostgreSQL.Simple.TypeInfo.Static (interval)
import Database.PostgreSQL.Simple.TypeInfo (typoid)
import Data.ByteString.Builder (doubleDec)
import Data.ByteString.Conversion (ToByteString)
import Data.Text.Encoding (decodeUtf8')
import Data.Thyme.Time (fromThyme, toThyme)
import Data.Thyme (UTCTime, formatTime, parseTime)
import Servant.API (FromHttpApiData)
import Servant.Missing (ThrowError500(..), MonadError500)
import System.Exit (ExitCode)
import System.Locale (defaultTimeLocale)
import Text.Email.Validate (EmailAddress, emailAddress, toByteString)
import Thentos.Prelude hiding ((.=))
import URI.ByteString (URI, RelativeRef, URIParseError,
                       uriAuthority, uriQuery, uriScheme, schemeBS, uriFragment, queryPairs,
                       parseURI, parseRelativeRef, laxURIParserOptions, serializeURI',
                       authorityHost, authorityPort, portNumber, hostBS, uriPath)
import Web.HttpApiData (parseQueryParam)

import qualified Data.Aeson as Aeson
import qualified Data.Csv as CSV
import qualified Data.ByteString as SBS
import qualified Data.HashMap.Strict as H
import qualified Data.Text as ST
import qualified Generics.Generic.Aeson as Aeson

import Thentos.CookieSession.Types

deriving instance ToField ThentosSessionToken
deriving instance FromField ThentosSessionToken

type Error500 = ThrowError500

-- * JSON helpers

-- | Variant of 'typeMismatch' that shows the offending value.
aesonError :: String -- ^ The name of the type you are trying to parse.
           -> Value  -- ^ The actual value encountered.
           -> Parser a
aesonError expected actual = fail $ "expected " ++ expected ++ ", encountered " ++ show actual

-- | Wrap a simple value in a JSON object with a single top-level field ("data").
-- This supports the convention that top-level JSON data structures should always be objects,
-- never arrays or simple values (e.g. strings).
newtype JsonTop a = JsonTop { fromJsonTop :: a }

instance (FromJSON a) => FromJSON (JsonTop a) where
    parseJSON (Aeson.Object (H.toList -> [("data", val)])) = JsonTop <$> Aeson.parseJSON val
    parseJSON bad = aesonError "JsonTop" bad

instance (ToJSON a) => ToJSON (JsonTop a) where
    toJSON (JsonTop val) = Aeson.object [ "data" .= Aeson.toJSON val]


-- * user

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

        -- FUTUREWORK: what we actually would want here is "something" (type?  function?  something
        -- more creative?)  that can be used as a filter on 'User' and will hide things from the
        -- service as appropriate.  we also want 'Service' to contain a counterpart "something".
        -- and a matcher that takes a service "something" and a user "something" and computes a
        -- compromise (or 'Nothing' if there is a conflict).

      }
  deriving (Eq, Show, Typeable, Generic)

newServiceAccount :: ServiceAccount
newServiceAccount = ServiceAccount False

newtype UserId = UserId { fromUserId :: Integer }
    deriving (Eq, Ord, Enum, Show, Read, Random, FromJSON, ToJSON, Typeable, Generic, FromHttpApiData,
              FromField, ToField)

newtype UserName = UserName { fromUserName :: ST }
    deriving (Eq, Ord, Show, Read, FromJSON, ToJSON, Typeable, Generic, IsString, FromField,
              ToField)

instance CSV.ToField UserName where
    toField = CSV.toField . fromUserName

instance CSV.FromField UserName where
    parseField = pure . UserName . cs

-- | BUG #399: ToJSON instance should go away in order to avoid accidental leakage of cleartext
-- passwords.  but for the experimentation phase this is too much of a headache.  (Under no
-- circumstances render to something like "[password hidden]".  Causes a lot of confusion.)
newtype UserPass = UserPass { fromUserPass :: ST }
    deriving (Eq, FromJSON, ToJSON, Typeable, Generic, IsString)

-- | The minimum length enforced when setting passwords.
minPasswordLength :: Int
minPasswordLength = 6

data HashedSecret a = BCryptHash SBS | SCryptHash SBS
    deriving (Eq, Show, Generic)

instance (Typeable a) => FromField (HashedSecret a) where
    fromField f = maybe (returnError Incompatible f "") parsePrefix
      where
        parsePrefix :: SBS -> Conversion (HashedSecret a)
        parsePrefix s = case SBS.splitAt 2 s of
            ("S_", h) -> return $ SCryptHash h
            ("B_", h) -> return $ BCryptHash h
            _         -> returnError ConversionFailed f ""

instance ToField (HashedSecret a) where
    toField (SCryptHash s) = toField $ "S_" <> s
    toField (BCryptHash s) = toField $ "B_" <> s

newtype UserEmail = UserEmail { userEmailAddress :: EmailAddress }
    deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance FromField UserEmail where
    fromField f Nothing = returnError UnexpectedNull f ""
    fromField f (Just bs) = case parseUserEmail (cs bs) of
                              Nothing -> returnError ConversionFailed f ""
                              Just e  -> return e

instance ToField UserEmail where
    toField = toField . fromUserEmail

instance CSV.ToField UserEmail where
    toField = toByteString . userEmailAddress

instance CSV.FromField UserEmail where
    parseField s = case parseUserEmail (cs s) of
        Just e -> pure e
        Nothing -> mzero

parseUserEmail :: ST -> Maybe UserEmail
parseUserEmail t = do
    email <- emailAddress (cs t)
    return $ UserEmail email

fromUserEmail :: UserEmail -> ST
fromUserEmail = cs . toByteString . userEmailAddress

instance FromJSON UserEmail
  where
    parseJSON (String (emailAddress . cs -> Just email)) = return $ UserEmail email
    parseJSON bad = aesonError "UserEmail" bad

instance ToJSON UserEmail
    where toJSON = Aeson.toJSON . fromUserEmail

-- UserEmail wrapped in an 'email' object (as JSON representation)
newtype WrappedEmail = WrappedEmail UserEmail
    deriving (Eq, Ord, Show, Read)

instance FromJSON WrappedEmail where
    parseJSON (Aeson.Object (H.toList -> [("email", val)])) = WrappedEmail <$> Aeson.parseJSON val
    parseJSON bad                                           = aesonError "WrappedEmail" bad

instance ToJSON WrappedEmail where
    toJSON (WrappedEmail email) = Aeson.object [ "email" .= email]


newtype ConfirmationToken = ConfirmationToken { fromConfirmationToken :: ST }
    deriving (Eq, Ord, Show, Read, Typeable, Generic, ToField, FromField, IsString)

instance FromJSON ConfirmationToken where
    parseJSON = Aeson.withText "confirmation token" (pure . ConfirmationToken)

instance ToJSON ConfirmationToken where toJSON (ConfirmationToken tok) = Aeson.toJSON tok


-- FIXME: why are some FromHttpApiData instances in this module derived and some explicitly defined?
instance FromHttpApiData ConfirmationToken where
    parseQueryParam tok = ConfirmationToken <$>
        either (Left . cs . show) Right (decodeUtf8' $ cs tok)

newtype PasswordResetToken = PasswordResetToken { fromPasswordResetToken :: ST }
    deriving (Eq, Ord, Show, Read, Typeable, Generic, IsString, FromField, ToField)

instance FromHttpApiData PasswordResetToken where
    parseQueryParam tok = PasswordResetToken <$>
        either (Left . cs . show) Right (decodeUtf8' $ cs tok)

instance FromJSON PasswordResetToken where
    parseJSON = Aeson.withText "PasswordResetToken" (pure . PasswordResetToken)

instance ToJSON PasswordResetToken where toJSON (PasswordResetToken tok) = Aeson.toJSON tok


data PasswordResetRequest = PasswordResetRequest
    { prPath     :: PasswordResetToken
    , prPassword :: UserPass
    }
    deriving (Eq)

instance FromJSON PasswordResetRequest where
    parseJSON (Aeson.Object m) = PasswordResetRequest <$> m .: "path" <*> m .: "password"
    parseJSON bad = aesonError "PasswordResetRequest" bad

instance ToJSON PasswordResetRequest where
    toJSON (PasswordResetRequest path pass) =
        Aeson.object [ "path" .= Aeson.toJSON path
                     , "password" .= Aeson.toJSON pass
                     ]


-- | Information required to create a new User
data UserFormData =
    UserFormData
        { udName     :: !UserName
        , udPassword :: !UserPass
        , udEmail    :: !UserEmail
        }
    deriving (Eq, Typeable, Generic)

instance FromJSON UserFormData where parseJSON = Aeson.gparseJson
instance ToJSON UserFormData where toJSON = Aeson.gtoJson

data UserCreationRequest = UserCreationRequest
    { ucUser    :: UserFormData
    , ucCaptcha :: CaptchaSolution
    }
    deriving (Eq, Typeable, Generic)

instance FromJSON UserCreationRequest where parseJSON = Aeson.gparseJson
instance ToJSON UserCreationRequest where toJSON = Aeson.gtoJson

data LoginFormData =
    LoginFormData
        { ldName     :: !UserName
        , ldPassword :: !UserPass
        }
    deriving (Eq, Typeable, Generic)

instance FromJSON LoginFormData where parseJSON = Aeson.gparseJson
instance ToJSON LoginFormData where toJSON = Aeson.gtoJson


-- * service

-- | (Service owner is an 'Agent', not a 'User', so that services can (but do not have to) be owned
-- by their parent services in a service hierarchy.)
data Service =
    Service
      { _serviceKey            :: !(HashedSecret ServiceKey)
      , _serviceOwner          :: !UserId
      , _serviceThentosSession :: !(Maybe ThentosSessionToken)
          -- ^ Used by the service to authenticate in communication with thentos.
      , _serviceName           :: !ServiceName
      , _serviceDescription    :: !ServiceDescription
      }
  deriving (Eq, Show, Typeable, Generic)

newtype ServiceId = ServiceId { fromServiceId :: ST }
  deriving (Eq, Ord, Show, Read, Typeable, Generic, IsString, FromHttpApiData, FromField, ToField)

instance FromJSON ServiceId where parseJSON = Aeson.gparseJson
instance ToJSON ServiceId where toJSON = Aeson.gtoJson

newtype ServiceKey = ServiceKey { fromServiceKey :: ST }
  deriving (Eq, Ord, Show, Read, Typeable, Generic, IsString)

instance FromJSON ServiceKey where parseJSON = Aeson.gparseJson
instance ToJSON ServiceKey where toJSON = Aeson.gtoJson

newtype ServiceName = ServiceName { fromServiceName :: ST }
  deriving (Eq, Ord, Show, Read, Typeable, Generic, IsString, FromHttpApiData, FromField, ToField)

instance FromJSON ServiceName where parseJSON = Aeson.gparseJson
instance ToJSON ServiceName where toJSON = Aeson.gtoJson

newtype ServiceDescription = ServiceDescription { fromServiceDescription :: ST }
  deriving (Eq, Ord, Show, Read, Typeable, Generic, IsString, FromHttpApiData, FromField, ToField)

instance FromJSON ServiceDescription where parseJSON = Aeson.gparseJson
instance ToJSON ServiceDescription where toJSON = Aeson.gtoJson

-- | Service-side authoriziation classes.  (For thentos-internal authorization classes, see 'Group'.)
--
-- Groups are opaque strings that services can use to manage authorizations for their users in
-- thentos.  One reason why thentos offers this (rather than leaving the groups-to-users mapping to
-- the internals of the service) is that this puts us in a position to do anonymized authentication:
-- we can assert a request is issued by a user member in a certain group, but not leak the name of
-- the user.
newtype ServiceGroup = ServiceGroup { fromGroup :: ST }
    deriving (Eq, Ord, Show, Read, Typeable, Generic, IsString, FromField, ToField)

instance FromJSON ServiceGroup where parseJSON = Aeson.withText "group string" (pure . ServiceGroup)

instance ToJSON ServiceGroup where toJSON (ServiceGroup name) = Aeson.toJSON name


-- * persona and context

newtype PersonaId = PersonaId { fromPersonaId :: Integer }
    deriving (Eq, Ord, Enum, Show, Read, Random, FromJSON, ToJSON, Typeable, Generic, FromHttpApiData,
              FromField, ToField)

newtype PersonaName = PersonaName { fromPersonaName :: ST }
    deriving (Eq, Ord, Show, Read, FromJSON, ToJSON, Typeable, Generic, IsString, FromField,
              ToField)

-- | *Note on the external url field:* Since personas are exposed to the service, it sometimes makes
-- sense for a service to maintain its own data item for each persona in thentos.  The persona's
-- external url can be used to point to that data item's rest url.
data Persona = Persona
  { _personaId          :: PersonaId
  , _personaName        :: PersonaName
  , _personaUid         :: UserId
  , _personaExternalUrl :: Maybe Uri
  } deriving (Eq, Show, Typeable, Generic)

newtype ContextId = ContextId { fromContextId :: Integer }
    deriving (Eq, Ord, Enum, Show, Read, Random, FromJSON, ToJSON, Typeable, Generic, FromHttpApiData,
              FromField, ToField)

newtype ContextName = ContextName { fromContextName :: ST }
    deriving (Eq, Ord, Show, Read, FromJSON, ToJSON, Typeable, Generic, IsString, FromField,
              ToField)

newtype ContextDescription = ContextDescription { fromContextDescription :: ST }
    deriving (Eq, Ord, Show, Read, FromJSON, ToJSON, Typeable, Generic, IsString, FromField,
              ToField)

data Context = Context
  { _contextId          :: ContextId
  , _contextService     :: ServiceId
  , _contextName        :: ContextName
  , _contextDescription :: ContextDescription
  , _contextUrl         :: Maybe ProxyUri
  } deriving (Eq, Show, Typeable, Generic)

instance Ord Context where
    compare = compare `on` _contextId


-- * thentos and service session

data ThentosSession =
    ThentosSession
      { _thSessAgent           :: !Agent
      , _thSessStart           :: !Timestamp
      , _thSessEnd             :: !Timestamp
      , _thSessExpirePeriod    :: !Timeout
      }
  deriving (Eq, Ord, Show, Typeable, Generic)

newtype ServiceSessionToken = ServiceSessionToken { fromServiceSessionToken :: ST }
    deriving (Eq, Ord, Show, Read, Typeable, Generic, IsString, FromHttpApiData, FromField, ToField)

instance FromJSON ServiceSessionToken where parseJSON = Aeson.gparseJson
instance ToJSON ServiceSessionToken where toJSON = Aeson.gtoJson

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

instance FromJSON ServiceSession where parseJSON = Aeson.gparseJson
instance ToJSON ServiceSession where toJSON = Aeson.gtoJson

data ServiceSessionMetadata =
    ServiceSessionMetadata
      { _srvSessMdUser :: !UserName
      }
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance FromJSON ServiceSessionMetadata where parseJSON = Aeson.gparseJson
instance ToJSON ServiceSessionMetadata where toJSON = Aeson.gtoJson

instance FromField ServiceSessionMetadata where
    fromField f dat = ServiceSessionMetadata <$> fromField f dat

data ByUserOrServiceId = ByUser UserId UserPass
                       | ByService ServiceId ServiceKey
  deriving (Eq, Typeable, Generic)

instance FromJSON ByUserOrServiceId where
    parseJSON (Aeson.Object (H.toList -> [(key, val)]))
        | key == "user"    = uncurry ByUser <$> Aeson.parseJSON val
        | key == "service" = uncurry ByService <$> Aeson.parseJSON val
    parseJSON bad = aesonError "ByUserOrServiceId" bad

instance ToJSON ByUserOrServiceId where
    toJSON (ByUser i k)    = Aeson.object [ "user" .= Aeson.toJSON (i, k)]
    toJSON (ByService i k) = Aeson.object [ "service" .= Aeson.toJSON (i, k)]


-- * send email

data EmailRecipients =
    EmailRecipients
      { _erEmails   :: ![UserEmail]
      , _erPersonas :: ![Uri]
      }
  deriving (Eq, Show, Typeable, Generic)

instance FromJSON EmailRecipients where
    parseJSON (Aeson.Object m) = EmailRecipients <$> m .: "emails" <*> m .: "personas"
    parseJSON bad = aesonError "EmailRecipients" bad

instance ToJSON EmailRecipients where
    toJSON (EmailRecipients emails personas) =
        Aeson.object [ "emails"   .= Aeson.toJSON emails
                     , "personas" .= Aeson.toJSON personas
                     ]

data SendEmailRequest =
    SendEmailRequest
      { _emailRecipients :: !EmailRecipients
      , _emailSubject    :: !ST
      , _emailBody       :: !ST
      , _emailHtml       :: !(Maybe ST)
      }
  deriving (Eq, Show, Typeable, Generic)

instance FromJSON SendEmailRequest where
    parseJSON (Aeson.Object m) = SendEmailRequest <$> m .: "recipients"
                                                  <*> m .: "subject"
                                                  <*> m .: "body"
                                                  <*> m .: "html"
    parseJSON bad = aesonError "SendEmailRequest" bad

instance ToJSON SendEmailRequest where
    toJSON (SendEmailRequest recipients subject body html) =
        Aeson.object [ "recipients" .= Aeson.toJSON recipients
                     , "subject"    .= Aeson.toJSON subject
                     , "body"       .= Aeson.toJSON body
                     , "html"       .= Aeson.toJSON html
                     ]


-- * timestamp, timeout

-- FIXME: move this section to module (Data.Timeout) in separate package `store-expire`.  (also move
-- gc transactions and possibly more?)  make it store-backend-agnostic (checkout `TCache` or file
-- upload in `warp`/`wai` for inspiration).  also possibly related: package `timeout`.

newtype Timestamp = Timestamp { fromTimestamp :: UTCTime }
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance ToField Timestamp where
    toField = toField . fromThyme . fromTimestamp

instance FromField Timestamp where
    fromField f dat = Timestamp . toThyme <$> fromField f dat

instance CSV.ToField Timestamp where
    toField = cs . timestampToString

instance CSV.FromField Timestamp where
    parseField = timestampFromString . cs

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

instance FromJSON Timestamp
  where
    parseJSON = (>>= timestampFromString) . Aeson.parseJSON

instance ToJSON Timestamp
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

instance FromJSON Timeout
  where
    parseJSON = (>>= timeoutFromString) . Aeson.parseJSON

instance ToJSON Timeout
  where
    toJSON = Aeson.toJSON . timeoutToString


-- * group, agent, lio

-- | Some thing or body that deals with (and can authenticate itself before) thentos.  Examples:
-- 'User' or 'Service'.  (We could have called this 'Principal', but that name is in use by LIO
-- already.)
data Agent = UserA !UserId | ServiceA !ServiceId
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance FromJSON Agent where parseJSON = Aeson.gparseJson
instance ToJSON Agent where toJSON = Aeson.gtoJson

-- | Thentos-internal authorization classes.  (See 'ServiceGroup' for service-side authorization classes.)
data Group =
    GroupAdmin
    -- ^ Can do anything.  (There may be no difference in behaviour from 'allowEverything'
    -- resp. 'thentosPublic', but if we ever want to restrict privileges, it's easier if it is a
    -- 'Group'.)

  | GroupUser
    -- ^ Can sign up with services

  | GroupUserAdmin
    -- ^ Can create (and manage her own) users

  | GroupServiceAdmin
    -- ^ Can create (and manage her own) services

  | GroupGroupAdmin
    -- ^ Can add personas and groups to groups and remove them
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Generic)

instance FromJSON Group where parseJSON = Aeson.gparseJson
instance ToJSON Group where toJSON = Aeson.gtoJson

instance ToCNF Agent where toCNF = toCNF . show
instance ToCNF Group where toCNF = toCNF . show

instance ToField Group where
    toField = toField . show

instance FromField Group where
    fromField f dat = do
        s <- fromField f dat
        case readMay s of
            Just r  -> return r
            Nothing -> returnError ConversionFailed f ""


-- * uri

-- | Wrapper around 'URI' with additional instance definitions.
newtype Uri = Uri { fromUri :: URI }
    deriving (Eq, Ord)

newtype RelRef = RelRef { fromRelRef :: RelativeRef }
    deriving (Eq, Ord)

instance FromHttpApiData RelRef where
    parseQueryParam s = case decodeUtf8' $ cs s of
        Right r -> over _Left (cs . show) $ RelRef <$> parseRelativeRef laxURIParserOptions (cs r)
        Left  e -> Left . cs . show $ e

parseUri :: SBS -> Either URIParseError Uri
parseUri bs = Uri <$> parseURI laxURIParserOptions bs

renderUri :: Uri -> SBS
renderUri (Uri uri) = serializeURI' uri

instance FromJSON Uri
  where
    parseJSON = Aeson.withText "URI string" $ either (fail . show) return . parseUri . cs

instance ToJSON Uri where toJSON uri = Aeson.toJSON (cs $ renderUri uri :: ST)

instance Show Uri where
    show = cs . renderUri

instance FromField Uri where
    fromField f Nothing = returnError UnexpectedNull f ""
    fromField f (Just bs) = either (returnError ConversionFailed f . show) return $ parseUri bs

instance ToField Uri where
    toField uri = toField (cs $ renderUri uri :: ST)


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

parseProxyUri :: forall m . MonadError String m => ST -> m ProxyUri
parseProxyUri t = case parseURI laxURIParserOptions $ cs t of
    Right uri -> do
        when (schemeBS (uriScheme uri) /= "http") $ fail_ "Expected http schema"
        unless (null . queryPairs $ uriQuery uri) $ fail_ "No query part allowed"
        unless (isNothing $ uriFragment uri) $ fail_ "No URI fragment allowed"
        auth <- maybe (fail_ "Missing URI authority") return $ uriAuthority uri
        let host = authorityHost auth
            port = fromMaybe 80 $ portNumber <$> authorityPort auth
        return ProxyUri { proxyHost = hostBS host
                        , proxyPort = port
                        , proxyPath = uriPath uri
                        }
    Left err -> fail_ $ "Invalid URI: " ++ show err
  where
    fail_ :: String -> m a
    fail_ = throwError . ("parseProxyUri: " ++)

instance FromJSON ProxyUri
  where
    parseJSON (String t) = either fail return $ parseProxyUri t
    parseJSON bad        = aesonError "ProxyUri" bad

instance ToJSON ProxyUri where
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
p <//> p' = cs $ stripTrailingSlash (cs p) <> "/" <> stripLeadingSlash (cs p')


-- * randomness

-- | 20 bytes of randomness.
-- For comparison: an UUID has 16 bytes, so that should be enough for all practical purposes.
newtype Random20 = Random20 SBS
    deriving (Eq, Ord, Show)

-- | Construct a 'Random20' from a bytestring. Returns 'Just' a Random20 wrapping the input
-- if its length is 20, 'Nothing' otherwise.
mkRandom20 :: SBS -> Maybe Random20
mkRandom20 bs = if SBS.length bs == 20 then Just $ Random20 bs else Nothing

-- | Extract the wrapped 20 bytes from a 'Random20'.
fromRandom20 :: Random20 -> SBS
fromRandom20 (Random20 bs) = bs


-- * binary data and captchas

-- FIXME: use the juicy-pixels type with the same name and meaning instead of making up a new one?
-- (or is there a differnce in meaning?)
newtype ImageData = ImageData { fromImageData :: SBS }
  deriving (Eq, Typeable, Generic)


newtype CaptchaId = CaptchaId { fromCaptchaId :: ST }
  deriving (Eq, Ord, Show, Read, Typeable, Generic, IsString, FromField, ToField, ToByteString)

instance FromJSON CaptchaId where
    parseJSON = Aeson.withText "captcha ID string" (pure . CaptchaId)

instance ToJSON CaptchaId where toJSON (CaptchaId cid) = Aeson.toJSON cid



data CaptchaSolution = CaptchaSolution
    { csId       :: CaptchaId
    , csSolution :: ST
    }
    deriving (Eq, Typeable, Generic, Show)

instance FromJSON CaptchaSolution where
    parseJSON (Aeson.Object m) = CaptchaSolution <$> m .: "id" <*> m .: "solution"
    parseJSON bad = aesonError "CaptchaSolution" bad

instance ToJSON CaptchaSolution where
    toJSON (CaptchaSolution cId cSol) =
        Aeson.object [ "id" .= Aeson.toJSON cId
                     , "solution" .= Aeson.toJSON cSol
                     ]

data CaptchaAttempt = CaptchaIncorrect | CaptchaCorrect
    deriving (Show, Eq, Ord)

instance CSV.ToField CaptchaAttempt where
    toField CaptchaCorrect = "1"
    toField CaptchaIncorrect = "0"

instance CSV.FromField CaptchaAttempt where
    parseField "1" = pure CaptchaCorrect
    parseField "0" = pure CaptchaIncorrect
    parseField _   = mzero

data SignupAttempt = SignupAttempt UserName UserEmail CaptchaAttempt Timestamp
    deriving (Show, Generic)

instance CSV.ToNamedRecord SignupAttempt where
    toNamedRecord (SignupAttempt name email captcha ts) =
        CSV.namedRecord [
            ("name", CSV.toField name),
            ("email", CSV.toField email),
            ("captcha_solved", CSV.toField captcha),
            ("timestamp", CSV.toField ts)
        ]

instance CSV.FromNamedRecord SignupAttempt where
    parseNamedRecord v = SignupAttempt <$>
            v CSV..: "name" <*>
            v CSV..: "email" <*>
            v CSV..: "captcha_solved" <*>
            v CSV..: "timestamp"

instance CSV.ToRecord SignupAttempt where
    toRecord (SignupAttempt n e c ts) =
        CSV.record [CSV.toField n, CSV.toField e, CSV.toField c, CSV.toField ts]

instance CSV.FromRecord SignupAttempt where
    parseRecord v
        | length v == 4 =
            SignupAttempt <$>
                v CSV..! 0 <*>
                v CSV..! 1 <*>
                v CSV..! 2 <*>
                v CSV..! 3
        | otherwise = mzero

instance CSV.DefaultOrdered SignupAttempt where
    headerOrder _ = CSV.header ["name", "email", "captcha_solved", "timestamp"]


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
    | GroupMembershipLoop ServiceGroup ServiceGroup
    | OperationNotPossibleInServiceSession
    | ServiceAlreadyExists
    | NotRegisteredWithService
    | UserEmailAlreadyExists
    | UserNameAlreadyExists
    | PersonaNameAlreadyExists
    | ContextNameAlreadyExists
    | CaptchaIdAlreadyExists
    | NoSuchCaptchaId
    | AudioCaptchaVoiceNotFound String
    | AudioCaptchaInternal ExitCode SBS SBS
    | BadCredentials
    | BadAuthenticationHeaders
    | ProxyNotAvailable
    | MissingServiceHeader
    | ProxyNotConfiguredForService ServiceId
    | NoSuchToken
    | NeedUserA ThentosSessionToken ServiceId
    | MalformedUserPath ST
    | InvalidCaptchaSolution
    | PasswordTooShort
    | OtherError e
    deriving (Eq, Read, Show, Typeable)

makePrisms ''ThentosError

instance (Show e, Typeable e) => Exception (ThentosError e)

type MonadThentosError e m = MonadError (ThentosError e) m
type MonadThentosIO m = MonadLIO DCLabel m

instance ThrowError500 err => ThrowError500 (ThentosError err) where
    error500 = _OtherError . error500


-- * boilerplate

makeLenses ''Persona
makeLenses ''Context
makeLenses ''Service
makeLenses ''ServiceAccount
makeLenses ''ServiceSession
makeLenses ''ThentosSession
makeLenses ''User
makeLenses ''EmailRecipients
makeLenses ''SendEmailRequest
