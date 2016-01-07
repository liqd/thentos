{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}

module Thentos.CaptchaService (main) where

import Control.Concurrent.MVar (MVar, newMVar)
import Control.Exception (finally)
import Control.Monad (void)
import "cryptonite" Crypto.Random (ChaChaDRG, drgNew)
import Data.Configifier ((>>.), Tagged(Tagged))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (cs)
import System.Log.Logger (Priority(INFO), removeAllHandlers)

import System.Log.Missing (logger, announceAction)
import Thentos (createConnPoolAndInitDb, runGcLoop)
import Thentos.Action.Types (ActionState(..))
import Thentos.Config
import Thentos.Smtp (checkSendmail)

import qualified Thentos.Backend.Api.Captcha as Captcha


-- * main

main :: IO ()
main = makeMain $ \actionState mBeConfig -> void $ mapM_ (`Captcha.runApi` actionState) mBeConfig


-- * main with abstract commands

makeMain :: (ActionState -> Maybe HttpConfig -> IO ()) -> IO ()
makeMain commandSwitch = do
    -- FIXME Refactor common code with Thentos.hs into functions and use those; eliminate
    -- unnecessary makeMain function
    config :: ThentosConfig <- getConfig "devel.config"
    -- FIXME checkEspeak instead
    checkSendmail (Tagged $ config >>. (Proxy :: Proxy '["smtp"]))

    rng :: MVar ChaChaDRG   <- drgNew >>= newMVar
    let dbName = config >>. (Proxy :: Proxy '["database", "name"])
    connPool <- createConnPoolAndInitDb $ cs dbName
    let actionState = ActionState config rng connPool
        logPath     = config >>. (Proxy :: Proxy '["log", "path"])
        logLevel    = config >>. (Proxy :: Proxy '["log", "level"])
    configLogger logPath logLevel
    _ <- runGcLoop actionState $ config >>. (Proxy :: Proxy '["gc_interval"])

    let mBeConfig :: Maybe HttpConfig
        mBeConfig = Tagged <$> config >>. (Proxy :: Proxy '["backend"])

    logger INFO "Press ^C to abort."
    let run = commandSwitch actionState mBeConfig
        finalize = announceAction "shutting down hslogger" removeAllHandlers

    run `finally` finalize
