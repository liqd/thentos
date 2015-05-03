{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Thentos.Frontend.Types where

import Control.Concurrent.MVar (MVar)
import Control.Lens (makeLenses, view)
import Control.Monad (mzero)
import Crypto.Random (SystemRNG)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Builder (toLazyByteString)
import Data.String.Conversions (ST, cs)
import GHC.Generics (Generic)
import Snap.Snaplet.AcidState (Acid, HasAcid(getAcidStore))
import Snap.Snaplet.Session.SessionManager (SessionManager)
import Snap.Snaplet (Snaplet, Handler, snapletValue)
import URI.ByteString (RelativeRef, serializeRelativeRef, parseRelativeRef, laxURIParserOptions)

import qualified Data.Aeson as Aeson
import qualified Generics.Generic.Aeson as Aeson

import Thentos.Types
import Thentos.Config

data FrontendApp =
    FrontendApp
      { _db :: Snaplet (Acid DB)
      , _rng :: MVar SystemRNG
      , _cfg :: ThentosConfig
      , _sess :: Snaplet SessionManager
      , _frontendCfg :: HttpConfig
      }

makeLenses ''FrontendApp

instance HasAcid FrontendApp DB where
    getAcidStore = view (db . snapletValue)

type FH = Handler FrontendApp FrontendApp

data FrontendSessionData =
    FrontendSessionData
        { fsdToken                :: SessionToken
        , fsdUser                 :: UserId
        , fsdServiceLoginCallback :: Maybe ServiceLoginCallback  -- ^ (see 'ServiceLoginCallback')
        , fsdMessages             :: [FrontendMsg]
        }
  deriving (Show, Eq, Generic)

instance FromJSON FrontendSessionData where parseJSON = Aeson.gparseJson
instance ToJSON FrontendSessionData where toJSON = Aeson.gtoJson

-- | If a user comes from a service login and is sent to the "register
-- with a new service" page because no valid 'ServiceAccount' exists
-- in thentos, an extra round of indirection is required (the user is
-- confronted with the service details and must say "yes, i want to
-- register", "share this-and-that data", etc.).  There may be further
-- extra rounds for logging in to thentos or registering with thentos.
-- This type is used to store the information from the service login
-- request in 'FrontendSessionData' until all these steps have been
-- taken.
newtype ServiceLoginCallback = ServiceLoginCallback (ServiceId, RelativeRef)
  deriving (Show, Eq, Generic)

instance ToJSON ServiceLoginCallback where
    toJSON (ServiceLoginCallback (mSid, rr)) = Aeson.toJSON (mSid, rr')
      where rr' = Aeson.String . cs . toLazyByteString . serializeRelativeRef $ rr

instance FromJSON ServiceLoginCallback where
    parseJSON v = do
        (mSid, rr' :: Aeson.Value) <- Aeson.parseJSON v
        case rr' of
            Aeson.String rr'' -> case parseRelativeRef laxURIParserOptions $ cs rr'' of
                (Right rr) -> return $ ServiceLoginCallback (mSid, rr)
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
