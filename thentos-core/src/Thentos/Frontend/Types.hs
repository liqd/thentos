{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

module Thentos.Frontend.Types where

import Control.Lens (makeLenses)
import Control.Monad (mzero)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Builder (toLazyByteString)
import Data.String.Conversions (ST, SBS, cs)
import GHC.Generics (Generic)
import URI.ByteString (RelativeRef, serializeRelativeRef, parseRelativeRef, laxURIParserOptions)

import qualified Data.Aeson as Aeson
import qualified Generics.Generic.Aeson as Aeson

import Thentos.Types
import Thentos.Action.Types (Action)


-- * frontend actions

type FAction = Action FActionError FrontendSessionData

data FActionError =
    FActionError303 SBS
  | FActionError404
  | FActionErrorNoToken
  | FActionErrorCreateService
  | FActionErrorServiceLoginNoCbUrl
  | FActionError500 String
  deriving (Eq, Show)


-- * session state

data FrontendSessionData =
    FrontendSessionData
        { _fsdLogin             :: Maybe FrontendSessionLoginData
        , _fsdServiceLoginState :: Maybe ServiceLoginState  -- ^ (see 'ServiceLoginState')
        , _fsdMessages          :: [FrontendMsg]
        }
  deriving (Show, Eq, Generic)

emptyFrontendSessionData :: FrontendSessionData
emptyFrontendSessionData = FrontendSessionData Nothing Nothing []

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
