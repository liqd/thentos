{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Thentos.Test.Types
where

import Control.Concurrent.Async (Async)
import Control.Lens (makeLenses)
import Network.HTTP.Types.Header (Header)
import Network.Wai (Application)

import qualified Test.WebDriver as WD

import Thentos.Action.Core
import Thentos.Config
import Thentos.Types


data TestConfig =
    TestConfig
      { _tcfgTmp                    :: FilePath
      , _tcfgDbPath                 :: FilePath
      , _tcfgServerFullBackendPort  :: Int
      , _tcfgServerFullFrontendPort :: Int
      , _tcfgWebdriverHost          :: String
      , _tcfgWebdriverPort          :: Int
      , _tcfgTraceHttp              :: Bool
      }
  deriving (Eq, Show)

$(makeLenses ''TestConfig)


-- | Basic Test State
data TS = TS
    { _tsCfg            :: TestConfig
    }

$(makeLenses ''TS)


-- | DB-Only Test State
data DBTS db = DBTS
    { _dbtsCfg            :: TestConfig
    , _dbtsActionState    :: ActionState db
    }

$(makeLenses ''DBTS)


-- | Backend Test State
data BTS db = BTS
    { _btsCfg            :: TestConfig
    , _btsActionState    :: ActionState db
    , _btsWai            :: Application
    , _btsToken          :: ThentosSessionToken
    , _btsGodCredentials :: [Header]
    , _btsServiceId      :: ServiceId
    }

$(makeLenses ''BTS)


-- | Frontend Test State
data FTS db = FTS
    { _ftsCfg         :: TestConfig
    , _ftsActionState :: ActionState db
    , _ftsBackend     :: Async ()  -- FIXME: capture stdout, stderr
    , _ftsBackendCfg  :: HttpConfig
    , _ftsFrontend    :: Async ()  -- FIXME: capture stdout, stderr
    , _ftsFrontendCfg :: HttpConfig
    , _ftsRunWD       :: forall a . WD.WD a -> IO a
    }

$(makeLenses ''FTS)
