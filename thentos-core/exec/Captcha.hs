{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Main (main) where

import Control.Concurrent.Async (concurrently)
import Control.Exception (finally)
import Control.Monad (void)
import Data.Configifier ((>>.), Tagged(Tagged))
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(Proxy))
import System.Log.Logger (Priority(INFO), removeAllHandlers)

import System.Log.Missing (logger, announceAction)
import Thentos (createConnPoolAndInitDb, runGcLoop, makeActionState)
import Thentos.Config
import Thentos.Sybil.AudioCaptcha (checkEspeak)

import qualified Thentos.Backend.Api.Captcha as Captcha


main :: IO ()
main = do
    config :: ThentosConfig <- getConfig "devel.config"
    checkEspeak  -- Make sure that we can successfully generate audio captchas
    connPool <- createConnPoolAndInitDb $ config >>. (Proxy :: Proxy '["database", "name"])
    actionState <- makeActionState config connPool
    configLogger . Tagged $ config >>. (Proxy :: Proxy '["log"])
    _ <- runGcLoop actionState $ config >>. (Proxy :: Proxy '["gc_interval"])

    let backendCfg  = forceCfg "backend" $ Tagged <$> config >>. (Proxy :: Proxy '["backend"])
        backend     = Captcha.runBackendApi backendCfg actionState
        frontendCfg = forceCfg "frontend" $ Tagged <$> config >>. (Proxy :: Proxy '["frontend"])
        frontend    = Captcha.runFrontendApi frontendCfg actionState
        run         = void $ concurrently backend frontend
        finalize    = announceAction "shutting down hslogger" removeAllHandlers

    logger INFO "Press ^C to abort."
    run `finally` finalize
  where
    forceCfg name = fromMaybe . error $ name ++ " not configured"
