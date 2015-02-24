{-# LANGUAGE OverloadedStrings                        #-}

module Main (main) where

import Data.Aeson (encode)
import Data.List (unfoldr)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))

import Network.HTTP.Conduit (Request(..), parseUrl, RequestBody(RequestBodyLBS))
import Network.HTTP.Conduit (Response)
import Network.HTTP.LoadTest.Types (Config(..), Req(..))
import Network.HTTP.Types.Header (RequestHeaders)
import Network.HTTP.Types.Method (methodPost)

import System.IO (stdout)
import System.Random (RandomGen, split, randoms, newStdGen)

import qualified Network.HTTP.LoadTest as Pronk
import qualified Network.HTTP.LoadTest.Report as Pronk
import qualified Network.HTTP.LoadTest.Analysis as Pronk
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T

import Thentos.Types (UserFormData(UserFormData), UserName(..), UserEmail(..))

main :: IO ()
main = do
    gen <- newStdGen
    let conf = pronkConfig $ mkSignupGens gen
    (Right summaryVector, time) <- Pronk.timed "foo" $ Pronk.run conf
    print time
    Pronk.reportBasic stdout $ Pronk.analyseBasic summaryVector time

pronkConfig :: [Pronk.RequestGenerator] -> Pronk.Config
pronkConfig reqs = Pronk.Config {
      concurrency = 100
    , numRequests = 1000
    , requestsPerSecond = 1000
    , timeout = 5
    , requests = reqs
    }

data BenchmarkConfig = BenchmarkConfig
    { targetHost :: String
    , targetBackendPort :: Int
    }

defaultBenchmarkConfig :: BenchmarkConfig
defaultBenchmarkConfig = BenchmarkConfig "localhost" 7001

makeRequest :: BenchmarkConfig -> String -> Request
makeRequest conf endpoint =
    (fromJust . parseUrl $
        "http://" ++ targetHost conf ++ ":"
        ++ show (targetBackendPort conf) ++ endpoint
    )
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
