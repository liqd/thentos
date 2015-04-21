{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Thentos.Frontend.Types where

import Control.Lens (makeLenses, view)
import Control.Monad (mzero)
import Control.Concurrent.MVar (MVar)
import Crypto.Random (SystemRNG)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Snap.Snaplet (Snaplet, Handler, snapletValue)
import Snap.Snaplet.AcidState (Acid, HasAcid(getAcidStore))
import Snap.Snaplet.Session.SessionManager (SessionManager)
import URI.ByteString  -- (RelativeRef)
import Data.ByteString.Builder (toLazyByteString)
import Data.String.Conversions (cs)

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
        , fsdServiceRegisterState :: Maybe ServiceRegisterState
        }
    deriving (Show, Eq, Generic)

instance FromJSON FrontendSessionData where parseJSON = Aeson.gparseJson
instance ToJSON FrontendSessionData where toJSON = Aeson.gtoJson

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
