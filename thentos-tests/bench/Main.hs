{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Main (main) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Lens ((^.))
import Data.Aeson (encode, decode)
import Data.List (unfoldr)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Conduit
    ( Request(..), RequestBody(RequestBodyLBS), Response(responseBody)
    , parseUrl, withManager, httpLbs )
import Network.HTTP.LoadTest.Types (Config(..), Req(..))
import Network.HTTP.Types.Method (methodPost, methodDelete)
import Safe (fromJustNote)
import System.IO (stdout)
import System.Random (RandomGen, split, randoms, newStdGen)
import Text.Show.Pretty (ppShow)

import qualified Codec.Binary.Base32 as Base32
import qualified Network.HTTP.LoadTest as Pronk
import qualified Network.HTTP.LoadTest.Report as Pronk
import qualified Network.HTTP.LoadTest.Analysis as Pronk
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as ST

import Thentos.Types
    ( UserFormData(UserFormData), UserName(..), UserPass(..), parseUserEmail
    , UserId, ThentosSessionToken(fromThentosSessionToken), UserId(..)
    )

import Thentos.Test.Core
import Thentos.Test.Types


main :: IO ()
main = do
    fts <- setupTestServerFull
    let cfg = fts ^. ftsCfg
    putStrLn $ ppShow cfg
    threadDelay $ let s = (* (1000 * 1000)) in s 2

    Just sessToken <- getThentosSessionToken cfg
    runSignupBench cfg sessToken
    runLoginBench cfg sessToken
    runCheckTokenBench cfg sessToken

    teardownTestServerFull fts

runSignupBench :: TestConfig -> ThentosSessionToken -> IO ()
runSignupBench cfg sessionToken = do
    gen <- newStdGen
    let conf = pronkConfig $ mkSignupGens cfg gen sessionToken
    runBench "Signup Benchmark" conf

runLoginBench :: TestConfig -> ThentosSessionToken -> IO ()
runLoginBench cfg sessionToken = do
    conf <- pronkConfig `fmap` mkLoginGens cfg sessionToken
    runBench "Login Benchmark" conf

runCheckTokenBench :: TestConfig -> ThentosSessionToken -> IO ()
runCheckTokenBench cfg sessionToken = do
    let conf = pronkConfig (repeat $ sessionCheckGen cfg sessionToken)
    runBench "Session-check Benchmark" conf

runBench :: ST.Text -> Pronk.Config -> IO ()
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

getThentosSessionToken :: TestConfig -> IO (Maybe ThentosSessionToken)
getThentosSessionToken cfg = do
    let (Just req_) = makeEndpoint cfg "/thentos_session"
        req = req_
                { requestBody = RequestBodyLBS $ encode (UserId 0, UserPass "god")
                , method = methodPost
                }
    withManager $ \m -> do
            resp <- httpLbs req m
            return $ decode (responseBody resp)

makeEndpoint :: TestConfig -> String -> Maybe Request
makeEndpoint cfg endpoint = f <$> parseUrl url
  where
    url = "http://localhost:" ++ show (cfg ^. tcfgServerFullBackendPort) ++ endpoint
    f req = req { requestHeaders = requestHeaders req ++ [("Content-Type", "application/json")] }

makeRequest :: TestConfig -> Maybe ThentosSessionToken -> String -> Request
makeRequest cfg mSession endpoint = req {requestHeaders = requestHeaders req ++ hdrs}
  where
    (Just req) = makeEndpoint cfg endpoint
    hdrs = case mSession of
        Nothing -> []
        Just token -> [("X-Thentos-Session", encodeUtf8 $ fromThentosSessionToken token)]


-- specific benchmarks

-- signup

signupGenTrans :: TestConfig -> ThentosSessionToken -> String -> (Req, Response LBS.ByteString -> String)
signupGenTrans cfg sessionToken charSource =
    let (cs . Base32.encode . cs -> name, remaining) = splitAt 30 charSource in
    let req = mkReq name in
    let cont = const remaining in
    (Req req, cont)
  where
    mkReq name =
        (makeRequest cfg (Just sessionToken) "/user")
            { method = methodPost
            , requestBody = RequestBodyLBS (encode formData)
            }
      where
        tName = ST.pack name
        uName = UserName tName
        uPass = "dummyPassword"
        uEmail = fromJustNote ("signupGenTrans: bad email: " <> show uEmail') $ parseUserEmail uEmail'
        uEmail' = tName <> "@example.com"
        formData = UserFormData uName uPass uEmail

mkSignupGens :: RandomGen r => TestConfig -> r -> ThentosSessionToken -> [Pronk.RequestGenerator]
mkSignupGens cfg r sessionToken =
    map
        (\s -> Pronk.RequestGeneratorStateMachine "Signup Generator"
                                                  (randoms s)
                                                  (signupGenTrans cfg sessionToken)
        )
        (unfoldr (Just . split) r)

-- login

data MachineState = MachineState !UserId !LoginState
data LoginState = LoggedOut | LoggedIn !ThentosSessionToken

loginGenTrans :: TestConfig -> MachineState -> (Req, Response LBS.ByteString -> MachineState)
loginGenTrans cfg (MachineState uid loginState) =
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
        (makeRequest cfg Nothing "/thentos_session")
            { method = methodPost
            , requestBody = RequestBodyLBS $ encode (uid, UserPass "dummyPassword")
            }

    logout tok = (logoutReq tok, const LoggedOut)

    logoutReq tok =
        (makeRequest cfg (Just tok) "/thentos_session")
            { method = methodDelete
            , requestBody = RequestBodyLBS $ encode tok
            }

mkLoginGens :: TestConfig -> ThentosSessionToken -> IO [Pronk.RequestGenerator]
mkLoginGens cfg sessionToken = do
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
            (loginGenTrans cfg)

    getUIDs :: IO (Maybe [UserId])
    getUIDs = do
        let req = makeRequest cfg (Just sessionToken) "/user"
        withManager $ \m -> do
            resp <- httpLbs req m
            return $ decode (responseBody resp)

-- checking session tokens
-- This is an intentionally trivial benchmark that is supposed to give
-- us a performance baseline by measuring mostly overhead (routing, json
-- decoding etc.)

sessionCheckGen :: TestConfig -> ThentosSessionToken -> Pronk.RequestGenerator
sessionCheckGen cfg sessionToken = Pronk.RequestGeneratorConstant $ Req req
  where
    req = (makeRequest cfg (Just sessionToken) "/thentos_session")
            {requestBody = RequestBodyLBS $ encode sessionToken }
