{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}

module Main (main) where

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
    -- FIXME: we need a function Thentos.Sybil.Captcha.init that is called here and crashes if
    -- espeak is not present on the system.  (it could also do other initialization IO like creating
    -- fonts in the future, but that wouldn't change the module surface much.)

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
