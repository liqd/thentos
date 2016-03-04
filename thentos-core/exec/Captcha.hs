{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Main (main) where

import Control.Concurrent.Async (concurrently)
import Control.Exception (finally)
import Data.Configifier ((>>.), Tagged(Tagged))

import Thentos (createConnPoolAndInitDb, runGcLoop, makeActionEnv)
import Thentos.Prelude
import Thentos.Config
import Thentos.Sybil.AudioCaptcha (checkEspeak)

import qualified Thentos.Backend.Api.Captcha as Captcha


main :: IO ()
main = do
    config :: ThentosConfig <- readConfig "devel.config"
    checkEspeak  -- Make sure that we can successfully generate audio captchas
    connPool <- createConnPoolAndInitDb config
    actionState <- makeActionEnv config connPool
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
