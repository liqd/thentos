{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DeriveGeneric          #-}
module Thentos.Frontend.Types where

import Control.Lens (makeLenses, view)
import Control.Concurrent.MVar (MVar)
import Crypto.Random (SystemRNG)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Snap.Snaplet (Snaplet, Handler, snapletValue)
import Snap.Snaplet.AcidState (Acid, HasAcid(getAcidStore))
import Snap.Snaplet.Session.SessionManager (SessionManager)

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
        { fsdToken :: SessionToken
        , fsdUser  :: UserId
        }
    deriving Generic

instance FromJSON FrontendSessionData where parseJSON = Aeson.gparseJson
instance ToJSON FrontendSessionData where toJSON = Aeson.gtoJson
