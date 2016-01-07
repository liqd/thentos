{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}

module Thentos.CaptchaService (main) where

import Control.Exception (finally)
import Data.Configifier ((>>.), Tagged(Tagged))
import Data.Proxy (Proxy(Proxy))
import System.Log.Logger (Priority(INFO), removeAllHandlers)

import System.Log.Missing (logger, announceAction)
import Thentos (createConnPoolAndInitDb, runGcLoop, makeActionState)
import Thentos.Config
import Thentos.Smtp (checkSendmail)

import qualified Thentos.Backend.Api.Captcha as Captcha


main :: IO ()
main = do
    config :: ThentosConfig <- getConfig "devel.config"
    -- FIXME checkEspeak instead
    checkSendmail (Tagged $ config >>. (Proxy :: Proxy '["smtp"]))

    connPool <- createConnPoolAndInitDb $ config >>. (Proxy :: Proxy '["database", "name"])
    actionState <- makeActionState config connPool
    configLogger . Tagged $ config >>. (Proxy :: Proxy '["log"])

    _ <- runGcLoop actionState $ config >>. (Proxy :: Proxy '["gc_interval"])

    case Tagged <$> config >>. (Proxy :: Proxy '["backend"]) of
        Nothing -> return () -- TODO: error message
        Just backendConfig -> do
            logger INFO "Press ^C to abort."
            let run = Captcha.runApi backendConfig actionState
                finalize = announceAction "shutting down hslogger" removeAllHandlers

            run `finally` finalize
