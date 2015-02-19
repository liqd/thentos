{-# LANGUAGE OverloadedStrings                        #-}

module Test.LoadTests (runLoadTest) where

import Data.Maybe (fromJust)

import Network.HTTP.LoadTest (run)
import Network.HTTP.LoadTest.Types (Config(..), Req(..))
import Network.HTTP.Conduit (Request(..), parseUrl)
import Network.HTTP.Types.Header (RequestHeaders)

runLoadTest :: IO ()
runLoadTest = return ()

config :: Config
config = Config {
      concurrency = 10
    , numRequests = 10000
    , requestsPerSecond = 1000
    , timeout = 5
    , request = Req baseRequest
    }

baseRequest :: Request
baseRequest =
    (fromJust $ parseUrl "http://localhost:7001/user/") { requestHeaders = thentosHeaders }

thentosHeaders :: RequestHeaders
thentosHeaders =
    [ ("X-Thentos-User", "god")
    , ("X-Thentos-Password", "god")
    ]
