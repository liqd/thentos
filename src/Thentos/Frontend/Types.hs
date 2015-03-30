{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Thentos.Frontend.Types where

import Control.Lens (makeLenses, view)
import Control.Concurrent.MVar (MVar)
import Crypto.Random (SystemRNG)
import Snap.Snaplet (Snaplet, snapletValue)
import Snap.Snaplet.AcidState (Acid, HasAcid(getAcidStore))
import Snap.Snaplet.Session.SessionManager (SessionManager)

import Thentos.Types
import Thentos.Config

data FrontendApp =
    FrontendApp
      { _db :: Snaplet (Acid DB)
      , _rng :: MVar SystemRNG
      , _cfg :: ThentosConfig
      , _sess :: Snaplet SessionManager
      }

makeLenses ''FrontendApp

instance HasAcid FrontendApp DB where
    getAcidStore = view (db . snapletValue)
