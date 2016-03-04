{-# LANGUAGE ConstraintKinds             #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE RankNTypes                  #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}

module Thentos.Frontend.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Builder (toLazyByteString)
import URI.ByteString (RelativeRef, serializeRelativeRef, parseRelativeRef, laxURIParserOptions)

import qualified Data.Aeson as Aeson
import qualified Generics.Generic.Aeson as Aeson

import Thentos.Prelude
import Thentos.Types
import Thentos.Action.Types


type MonadThentosFError m = MonadThentosError FActionError m
type MonadThentosFState m = MonadState FrontendSessionData m
type MonadFAction m =
    (MonadReader ActionEnv m,
     MonadThentosError FActionError m,
     MonadState FrontendSessionData m,
     MonadRandom m,
     MonadThentosIO m)
type FAction a = forall m. MonadFAction m => m a
type FormHandler f = forall m. MonadFAction m => f m
-- * frontend errors

data FActionError =
    FActionError303 SBS
  | FActionError404
  | FActionErrorNoToken
  | FActionErrorCreateService
  | FActionErrorServiceLoginNoCbUrl
  | FActionError500 String
  deriving (Eq, Show)

crash :: FActionError -> FAction a
crash = throwError . OtherError

-- * session state

data FrontendSessionData =
    FrontendSessionData
        { _fsdLogin             :: Maybe FrontendSessionLoginData
        , _fsdServiceLoginState :: Maybe ServiceLoginState  -- ^ (see 'ServiceLoginState')
        , _fsdCsrfToken         :: Maybe CsrfToken -- ^ (see 'CsrfToken')
        , _fsdMessages          :: [FrontendMsg]
        }
  deriving (Show, Eq, Generic)

emptyFrontendSessionData :: FrontendSessionData
emptyFrontendSessionData = FrontendSessionData Nothing Nothing Nothing []

data FrontendSessionLoginData =
    FrontendSessionLoginData
        { _fslToken        :: ThentosSessionToken
        , _fslUserId       :: UserId
        , _fslDashboardTab :: Maybe DashboardTab
        }
  deriving (Show, Eq, Generic)

data DashboardTab =
    DashboardTabDetails
  | DashboardTabServices
  | DashboardTabOwnServices
  | DashboardTabUsers
  | DashboardTabLogout
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

instance FromJSON FrontendSessionData where parseJSON = Aeson.gparseJson
instance ToJSON FrontendSessionData where toJSON = Aeson.gtoJson

instance FromJSON FrontendSessionLoginData where parseJSON = Aeson.gparseJson
instance ToJSON FrontendSessionLoginData where toJSON = Aeson.gtoJson

instance FromJSON DashboardTab where parseJSON = Aeson.gparseJson
instance ToJSON DashboardTab where toJSON = Aeson.gtoJson

-- | If a user comes from a service login and is sent to the "register
-- with a new service" page because no valid 'ServiceAccount' exists
-- in thentos, an extra round of indirection is required (the user is
-- confronted with the service details and must say "yes, i want to
-- register", "share this-and-that data", etc.).  There may be further
-- extra rounds for logging in to thentos or registering with thentos.
-- This type is used to store the information from the service login
-- request in 'FrontendSessionData' until all these steps have been
-- taken.
data ServiceLoginState =
    ServiceLoginState
        { _fslServiceId :: ServiceId
        , _fslRR        :: RelativeRef  -- ^ e.g. @/service/login?...@
            -- FIXME: give fslRR a better name.  something that explains the meaning of where it is
            -- pointing.
        }
  deriving (Show, Eq, Generic)

instance ToJSON ServiceLoginState where
    toJSON (ServiceLoginState mSid rr) = Aeson.toJSON (mSid, rr')
      where rr' = Aeson.String . cs . toLazyByteString . serializeRelativeRef $ rr

instance FromJSON ServiceLoginState where
    parseJSON v = do
        (mSid, rr' :: Aeson.Value) <- Aeson.parseJSON v
        case rr' of
            Aeson.String rr'' -> case parseRelativeRef laxURIParserOptions $ cs rr'' of
                (Right rr) -> return $ ServiceLoginState mSid rr
                _ -> mzero
            _ -> mzero

-- | This token is used to prevent CSRF (Cross Site Request Forgery).
-- This token is part of 'FrontendSessionData' since it is required by the views which
-- generate the forms with a special hidden field containing the value of this token.
-- However, this token is cleared before being serialized as a cookie.
-- Indeed we have no need yet to have it on the client side nor to make it persistent.
-- When processing requests, this token is freshly generated from the 'CsrfSecret' and the
-- 'ThentosSessionToken'. This token is only used by requests that yield an HTML form.
-- Upon POST requests on such forms, the handlers will check the validity of the CSRF token.
-- Verification of this token can be done solely from the 'CsrfSecret' and
-- the 'ThentosSessionToken'.
--
-- This all means that if the attacker could get access to one of these tokens it would be enough to
-- validate any form.  Changing the token on every request even inside the session helps to counter
-- an attack based on entropy leakage through TLS plaintext compression.  TLS compression should be
-- disabled for the exact reason that it is vulnerable to this attack, but this code does not rely
-- on it.
--
-- *The idea of the attack:* When combining encryption (which hides the contents but not the length)
-- and compression (which makes the length depend on the content).  If you compress after encryption
-- its safe but useless; if you compress then encrypt (which is often done), then some data leaks
-- through the length of the ciphertext.  The BEAST attack exploited this by guessing the CSRF token
-- by sending request to the server pretending to be the client injecting in the request the guess
-- of the token in a parameter which is echoed back by the server to the real client encrypted for
-- the client.  Assuming the CSRF token is given as an attribute such as csrf:SOMESECRET to keep it
-- simple, then the guess is going to be csrf:XYZ with all combination of XYZ then you look at which
-- answer was the shortest it is highly likely that XYZ=SOM will compress better than the rest
-- because of the repetition with the real secret also part of the response. You then proceed with
-- your guess being csrf:SOMXYZ. On a test setup, it was possible to recover the full token in 30
-- secs.
newtype CsrfToken = CsrfToken { fromCsrfToken :: ST }
    deriving (Eq, Ord, Show, Read, FromJSON, ToJSON, Typeable, Generic, IsString)

-- | If we want to communicate information from the last request to
-- the user in the next one, we need to stash it in the state.  This
-- is the type for that.
data FrontendMsg =
    FrontendMsgError ST
  | FrontendMsgSuccess ST
  deriving (Show, Eq, Generic)

instance FromJSON FrontendMsg where parseJSON = Aeson.gparseJson
instance ToJSON FrontendMsg where toJSON = Aeson.gtoJson


makeLenses ''FrontendSessionData
makeLenses ''FrontendSessionLoginData
makeLenses ''ServiceLoginState
