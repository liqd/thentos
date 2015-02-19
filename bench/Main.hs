{-# LANGUAGE OverloadedStrings                        #-}

module Main (main) where

import Data.Maybe (fromJust)
import Data.Aeson (encode)
import Data.Monoid ((<>))

import qualified Network.HTTP.LoadTest as Pronk
import Network.HTTP.LoadTest.Types (Config(..), Req(..))
import qualified  Network.HTTP.LoadTest.Types as Pronk
import Network.HTTP.Conduit (Request(..), parseUrl, RequestBody(RequestBodyLBS))
import Network.HTTP.Types.Header (RequestHeaders)
import Network.HTTP.Types.Method (methodPost)

import System.Random (RandomGen, split, randoms, newStdGen)
import Data.List (unfoldr)
import Network.HTTP.Conduit (Response)
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T


import Util (timed)

import Thentos.Types (UserFormData(UserFormData), UserName(..), UserEmail(..))

main :: IO ()
main = do
    gen <- newStdGen
    let conf = pronkConfig $ mkSignupGens gen
    (run, time) <- timed $ Pronk.run conf
    print time
    print run

pronkConfig :: [Pronk.RequestGenerator] -> Pronk.Config
pronkConfig reqs = Pronk.Config {
      concurrency = 10
    , numRequests = 10000
    , requestsPerSecond = 1000
    , timeout = 5
    , requests = reqs
    }

data BenchmarkConfig = BenchmarkConfig
    { targetHost :: String
    , targetFrontendPort :: Int
    , targetBackendPort :: Int
    }

defaultBenchmarkConfig :: BenchmarkConfig
defaultBenchmarkConfig = BenchmarkConfig "localhost" 7002 7001

makeRequest :: BenchmarkConfig -> String -> Request
makeRequest (BenchmarkConfig host _ bPort) path =
    (fromJust . parseUrl $ "http://" ++ host ++ ":" ++ show bPort ++ path)
        { requestHeaders = thentosHeaders }

thentosHeaders :: RequestHeaders
thentosHeaders =
    [ ("X-Thentos-User", "god")
    , ("X-Thentos-Password", "god")
    ]


-- specific benchmarks
signupGenTrans :: [Char] -> (Req, Response L.ByteString -> [Char])
signupGenTrans charSource =
    let (name, remaining) = splitAt 10 charSource in
    let req = mkReq name in
    let cont = const remaining in
    (Req req, cont)
  where
    mkReq name =
        let tName = T.pack name in
        let formData = UserFormData (UserName tName)
                                    "dummyPassword"
                                    (UserEmail $ tName <> "@example.com") in
        (makeRequest defaultBenchmarkConfig "/user")
            { method = methodPost
            , requestBody = RequestBodyLBS (encode formData)
            }

mkSignupGens :: RandomGen r => r -> [Pronk.RequestGenerator]
mkSignupGens r =
    map
        (\s -> Pronk.RequestGeneratorStateMachine "Signup Generator"
                                                  (randoms s)
                                                  signupGenTrans
        )
        (unfoldr (Just . split) r)
