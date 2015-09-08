module Thentos.Adhocracy3.Backend.Api.Proxy where

import Data.Aeson (encode)
import System.Log.Logger (Priority(CRITICAL))
import Thentos.Backend.Api.Proxy
import Thentos.Adhocracy3.Types

-- | An exception handler that responds with an A3 error message with 500
-- status code to all exceptions.
proxyExceptionHandler :: SomeException -> S.Application
proxyExceptionHandler e _ respond = do
    logger'P CRITICAL $ "Proxy error: " ++ show e
    respond $ S.responseLBS status500 (encode body) ct
  where
    ct = [("Content-Type", "application/json")]
    body = A3ErrorMessage [A3Error { aeName   = "server error"
                                   , aeLocation = "none"
                                   , aeDescription = "Internal server error"
                                   }]
