{-# LANGUAGE OverloadedStrings                        #-}

module Main (main) where

import Data.Aeson (encode, decode)
import Data.List (unfoldr)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8)

import Network.HTTP.Conduit (Request(..), parseUrl, RequestBody(RequestBodyLBS))
import Network.HTTP.Conduit (Response(responseBody), withManager, httpLbs)
import Network.HTTP.LoadTest.Types (Config(..), Req(..))
import Network.HTTP.Types.Method (methodPost, methodDelete)

import System.IO (stdout)
import System.Random (RandomGen, split, randoms, newStdGen)

import qualified Network.HTTP.LoadTest as Pronk
import qualified Network.HTTP.LoadTest.Report as Pronk
import qualified Network.HTTP.LoadTest.Analysis as Pronk
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T

import Thentos.Types (UserFormData(UserFormData), UserName(..), UserEmail(..),
    UserId, SessionToken(fromSessionToken), UserId(..), UserPass(..))

main :: IO ()
main = do
    Just sessToken <- getSessionToken defaultBenchmarkConfig
    runSignupBench sessToken
    runLoginBench sessToken
    runCheckTokenBench sessToken

runSignupBench :: SessionToken -> IO ()
runSignupBench sessionToken = do
    gen <- newStdGen
    let conf = pronkConfig $ mkSignupGens gen sessionToken
    runBench "Signup Benchmark" conf

runLoginBench :: SessionToken -> IO ()
runLoginBench sessionToken = do
    conf <- pronkConfig `fmap` mkLoginGens sessionToken
    runBench "Login Benchmark" conf

runCheckTokenBench :: SessionToken -> IO ()
runCheckTokenBench sessionToken = do
    let conf = pronkConfig (repeat $ sessionCheckGen sessionToken)
    runBench "Session-check Benchmark" conf

runBench :: T.Text -> Pronk.Config -> IO ()
runBench benchmarkName conf = do
    (Right summaryVector, time) <- Pronk.timed benchmarkName $ Pronk.run conf
    Pronk.reportBasic stdout $ Pronk.analyseBasic summaryVector time

pronkConfig :: [Pronk.RequestGenerator] -> Pronk.Config
pronkConfig reqs = Pronk.Config {
      concurrency = 100
    , numRequests = 1000
    , requestsPerSecond = 1000
    , timeout = 8
    , requests = reqs
    }

data BenchmarkConfig = BenchmarkConfig
    { targetBackendHost :: String
    , targetBackendPort :: Int
    }

defaultBenchmarkConfig :: BenchmarkConfig
defaultBenchmarkConfig = BenchmarkConfig "localhost" 7001

getSessionToken :: BenchmarkConfig -> IO (Maybe SessionToken)
getSessionToken conf = do
    let req = (fromJust . parseUrl $ "http://" ++ targetBackendHost conf ++ ":"
                        ++ show (targetBackendPort conf) ++ "/session"
              ) { requestBody = RequestBodyLBS $ encode (UserId 0, UserPass "god")
                , method = methodPost
                }
    withManager $ \m -> do
            resp <- httpLbs req m
            return $ decode (responseBody resp)

makeRequest :: Maybe SessionToken -> BenchmarkConfig -> String -> Request
makeRequest mSession conf endpoint =
    let r = (fromJust . parseUrl $
                "http://" ++ targetBackendHost conf ++ ":"
                ++ show (targetBackendPort conf) ++ endpoint
            ) in
    case mSession of
        Nothing -> r
        Just token ->
            r {requestHeaders = [("X-Thentos-Session",
                                  encodeUtf8 $ fromSessionToken token)]}

-- specific benchmarks

-- signup
signupGenTrans :: SessionToken -> [Char] -> (Req, Response L.ByteString -> [Char])
signupGenTrans sessionToken charSource =
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
        (makeRequest (Just sessionToken) defaultBenchmarkConfig "/user")
            { method = methodPost
            , requestBody = RequestBodyLBS (encode formData)
            }

mkSignupGens :: RandomGen r => r -> SessionToken -> [Pronk.RequestGenerator]
mkSignupGens r sessionToken =
    map
        (\s -> Pronk.RequestGeneratorStateMachine "Signup Generator"
                                                  (randoms s)
                                                  (signupGenTrans sessionToken)
        )
        (unfoldr (Just . split) r)

-- login

data MachineState = MachineState !UserId !LoginState
data LoginState = LoggedOut | LoggedIn !SessionToken

loginGenTrans :: MachineState -> (Req, Response L.ByteString -> MachineState)
loginGenTrans (MachineState uid loginState) =
    let (r, c) = case loginState of
            LoggedOut -> login
            LoggedIn token -> logout token
    in (Req r, MachineState uid . c)
  where
    login = (loginReq, loginCont)
    loginCont resp =
        let sessionToken = decode $ responseBody resp in
        LoggedIn $ fromMaybe (error "Got no session token") sessionToken

    loginReq =
        (makeRequest Nothing defaultBenchmarkConfig "/session")
            { method = methodPost
            , requestBody = RequestBodyLBS $ encode (uid, UserPass "dummyPassword")
            }

    logout tok = (logoutReq tok, const LoggedOut)

    logoutReq tok =
        (makeRequest (Just tok) defaultBenchmarkConfig "/session")
            { method = methodDelete
            , requestBody = RequestBodyLBS $ encode tok
            }

mkLoginGens :: SessionToken -> IO [Pronk.RequestGenerator]
mkLoginGens sessionToken = do
    Just uids <- getUIDs
    -- take out god user so all users have the same password
    let uids' = filter (\(UserId n) -> n /= 0) uids
    return . cycle $ map makeGenerator uids'
  where
    makeGenerator :: UserId -> Pronk.RequestGenerator
    makeGenerator uid =
        Pronk.RequestGeneratorStateMachine
            "Login"
            (MachineState uid LoggedOut)
            loginGenTrans

    getUIDs :: IO (Maybe [UserId])
    getUIDs = do
        let req = makeRequest (Just sessionToken) defaultBenchmarkConfig "/user"
        withManager $ \m -> do
            resp <- httpLbs req m
            return $ decode (responseBody resp)

-- checking session tokens
sessionCheckGen :: SessionToken -> Pronk.RequestGenerator
sessionCheckGen sessionToken = Pronk.RequestGeneratorConstant $ Req req
  where
    req = (makeRequest (Just sessionToken) defaultBenchmarkConfig "/session")
            {requestBody = RequestBodyLBS $ encode sessionToken }
