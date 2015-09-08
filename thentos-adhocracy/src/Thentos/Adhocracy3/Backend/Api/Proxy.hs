module Thentos.Adhocracy3.Backend.Api.Proxy where

import Thentos.Backend.Api.Proxy

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
