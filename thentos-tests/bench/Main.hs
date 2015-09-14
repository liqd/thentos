{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds           #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Data.Aeson (encode, decode)
import Data.Configifier ((>>.))
import Data.List (unfoldr)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (cs)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Conduit
    ( Request(..), RequestBody(RequestBodyLBS), Response(responseBody)
    , parseUrl, httpLbs, newManager, tlsManagerSettings )
import Network.HTTP.LoadTest.Types (Config(..), Req(..))
import Network.HTTP.Types.Method (methodPost, methodDelete)
import Safe (fromJustNote)
import System.IO (stdout)
import System.Random (RandomGen, split, randoms, newStdGen)

import qualified Codec.Binary.Base32 as Base32
import qualified Network.HTTP.LoadTest as Pronk
import qualified Network.HTTP.LoadTest.Report as Pronk
import qualified Network.HTTP.LoadTest.Analysis as Pronk
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as ST

import Thentos.Config (ThentosConfig)
import Thentos.Action.Core (ActionState(..))
import Thentos.Types
    ( UserFormData(UserFormData), UserName(..), UserPass(..), parseUserEmail
    , UserId, ThentosSessionToken(fromThentosSessionToken), UserId(..)
    )

import Thentos.Test.Config (godUid, godPass)
import Thentos.Test.Core


main :: IO ()
main = do
    withFrontendAndBackend $ \ (ActionState (_, _, cfg)) -> do
        -- putStrLn . cs . renderConfigFile $ cfg
        threadDelay $ let s = (* (1000 * 1000)) in s 2

        Just sessToken <- getThentosSessionToken cfg
        runSignupBench cfg sessToken
        runLoginBench cfg sessToken
        runCheckTokenBench cfg sessToken

runSignupBench :: ThentosConfig -> ThentosSessionToken -> IO ()
runSignupBench cfg sessionToken = do
    gen <- newStdGen
    let conf = pronkConfig $ mkSignupGens cfg gen sessionToken
    runBench "Signup Benchmark" conf

runLoginBench :: ThentosConfig -> ThentosSessionToken -> IO ()
runLoginBench cfg sessionToken = do
    conf <- pronkConfig `fmap` mkLoginGens cfg sessionToken
    runBench "Login Benchmark" conf

runCheckTokenBench :: ThentosConfig -> ThentosSessionToken -> IO ()
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

getThentosSessionToken :: ThentosConfig -> IO (Maybe ThentosSessionToken)
getThentosSessionToken cfg = do
    let (Just req_) = makeEndpoint cfg "/thentos_session"
        req = req_
                { requestBody = RequestBodyLBS $ encode (godUid, godPass)
                , method = methodPost
                }
    m <- newManager tlsManagerSettings
    resp <- httpLbs req m
    let respBody = decodeLenient . responseBody $ resp
    -- print (resp, respBody)
    return $ either (\_ -> Nothing) Just respBody

makeEndpoint :: ThentosConfig -> String -> Maybe Request
makeEndpoint cfg endpoint = f <$> parseUrl url
  where
    Just (Just (backendPort :: Int)) = cfg >>. (Proxy :: Proxy ["backend", "expose_port"])
    url = "http://localhost:" ++ show backendPort ++ endpoint
    f req = req { requestHeaders = requestHeaders req ++ [("Content-Type", "application/json")] }

makeRequest :: ThentosConfig -> Maybe ThentosSessionToken -> String -> Request
makeRequest cfg mSession endpoint = req {requestHeaders = requestHeaders req ++ hdrs}
  where
    (Just req) = makeEndpoint cfg endpoint
    hdrs = case mSession of
        Nothing -> []
        Just token -> [("X-Thentos-Session", encodeUtf8 $ fromThentosSessionToken token)]


-- specific benchmarks

-- signup

signupGenTrans :: ThentosConfig -> ThentosSessionToken -> String -> (Req, Response LBS.ByteString -> String)
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

mkSignupGens :: RandomGen r => ThentosConfig -> r -> ThentosSessionToken -> [Pronk.RequestGenerator]
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

loginGenTrans :: ThentosConfig -> MachineState -> (Req, Response LBS.ByteString -> MachineState)
loginGenTrans cfg (MachineState uid loginState) =
    let (r, c) = case loginState of
            LoggedOut -> login
            LoggedIn token -> logout token
    in (Req r, MachineState uid . c)
  where
    login = (loginReq, loginCont)
    loginCont resp =
        let Right sessionToken = decodeLenient $ responseBody resp in
        LoggedIn sessionToken

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

mkLoginGens :: ThentosConfig -> ThentosSessionToken -> IO [Pronk.RequestGenerator]
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
        m <- newManager tlsManagerSettings
        decode . responseBody <$> httpLbs req m

-- checking session tokens
-- This is an intentionally trivial benchmark that is supposed to give
-- us a performance baseline by measuring mostly overhead (routing, json
-- decoding etc.)

sessionCheckGen :: ThentosConfig -> ThentosSessionToken -> Pronk.RequestGenerator
sessionCheckGen cfg sessionToken = Pronk.RequestGeneratorConstant $ Req req
  where
    req = (makeRequest cfg (Just sessionToken) "/thentos_session")
            {requestBody = RequestBodyLBS $ encode sessionToken }
