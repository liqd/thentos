{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Test.Types
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
      , _tcfgRestPort               :: Int
      , _tcfgServerFullBackendPort  :: Int
      , _tcfgServerFullFrontendPort :: Int
      , _tcfgWebdriverHost          :: String
      , _tcfgWebdriverPort          :: Int
      }
  deriving (Eq, Show)

$(makeLenses ''TestConfig)


-- DB-Only Test State
data DBTS = DBTS
    { _dbtsCfg            :: TestConfig
    , _dbtsActionState    :: ActionState DB
    }

$(makeLenses ''DBTS)


-- Backend Test State
data BTS = BTS
    { _btsCfg            :: TestConfig
    , _btsActionState    :: ActionState DB
    , _btsWai            :: Application
    , _btsToken          :: ThentosSessionToken
    , _btsGodCredentials :: [Header]
    }

$(makeLenses ''BTS)


-- Frontend Test State
data FTS = FTS
    { _ftsCfg         :: TestConfig
    , _ftsActionState :: ActionState DB
    , _ftsBackend     :: Async ()  -- FIXME: capture stdout, stderr
    , _ftsBackendCfg  :: HttpConfig
    , _ftsFrontend    :: Async ()  -- FIXME: capture stdout, stderr
    , _ftsFrontendCfg :: HttpConfig
    , _ftsRunWD       :: forall a . WD.WD a -> IO a
    }

$(makeLenses ''FTS)
