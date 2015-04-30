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
import Data.String.Conversions (cs)
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
        , fsdServiceRegisterState :: Maybe ServiceRegisterState  -- ^ (see 'ServiceRegisterState')
        }
    deriving (Show, Eq, Generic)

instance FromJSON FrontendSessionData where parseJSON = Aeson.gparseJson
instance ToJSON FrontendSessionData where toJSON = Aeson.gtoJson

-- | If a user comes from a service login and is sent to the "register
-- with a new service" page because no valid account exists with
-- thentos, an extra round of in direction is required (the user is
-- confronted with the service details and must say "yes, i want to
-- register", "share this-and-that data", etc.).  This type is used to
-- store the information from the registration request in
-- 'FrontendSessionData' between the form rendering and the form
-- processing requests.
newtype ServiceRegisterState = ServiceRegisterState (RelativeRef, ServiceId)
  deriving (Show, Eq, Generic)

instance ToJSON ServiceRegisterState where
    toJSON (ServiceRegisterState (rr, mSid)) = Aeson.toJSON (rr', mSid)
      where rr' = Aeson.String . cs . toLazyByteString . serializeRelativeRef $ rr

instance FromJSON ServiceRegisterState where
    parseJSON v = do
        (rr' :: Aeson.Value, mSid) <- Aeson.parseJSON v
        case rr' of
            Aeson.String rr'' -> case parseRelativeRef laxURIParserOptions $ cs rr'' of
                (Right rr) -> return $ ServiceRegisterState (rr, mSid)
                _ -> mzero
            _ -> mzero
