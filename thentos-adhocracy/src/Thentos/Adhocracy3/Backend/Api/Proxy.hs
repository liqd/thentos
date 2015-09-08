{-# LANGUAGE OverloadedStrings #-}
module Thentos.Adhocracy3.Backend.Api.Proxy where

import Control.Exception (SomeException)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode)
import Network.HTTP.Types (status500)
import System.Log.Logger (Priority(CRITICAL))
import System.Log.Missing (logger)

import qualified Network.Wai as Wai

import Thentos.Backend.Api.Proxy
import Thentos.Adhocracy3.Types

-- | An exception handler that responds with an A3 error message with 500
-- status code to all exceptions.
proxyExceptionHandler :: SomeException -> Wai.Application
proxyExceptionHandler e _ respond = do
    logger CRITICAL $ "Proxy error: " ++ show e
    respond $! Wai.responseLBS status500 ct (encode body)
  where
    ct = [("Content-Type", "application/json")]
    body = A3ErrorMessage [A3Error { aeName   = "server error"
                                   , aeLocation = "none"
                                   , aeDescription = "Internal server error"
                                   }]
