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


-- Backend Test State
data BTS = BTS
    { _btsActionState    :: ActionState DB
    , _btsWai            :: Application
    , _btsToken          :: ThentosSessionToken
    , _btsGodCredentials :: [Header]
    }

$(makeLenses ''BTS)


-- Frontend Test State
data FTS = FTS
    { _ftsActionState :: ActionState DB
    , _ftsBackend     :: Async ()  -- FIXME: capture stdout, stderr
    , _ftsBackendCfg  :: HttpConfig
    , _ftsFrontend    :: Async ()  -- FIXME: capture stdout, stderr
    , _ftsFrontendCfg :: HttpConfig
    , _ftsRunWD       :: forall a . WD.WD a -> IO a
    }

$(makeLenses ''FTS)
