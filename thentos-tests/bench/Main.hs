{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -Wall #-}

module Main (main, runCaptchaBench) where

import Control.Concurrent (threadDelay)
import Database.PostgreSQL.Simple (Connection, query_, fromOnly)
import Data.Configifier ((>>.))
import Data.Functor.Infix ((<$$>))
import Data.List (unfoldr)
import Data.Pool (Pool, withResource)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (LBS, cs, (<>))
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
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Parser as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as ST
import qualified Network.HTTP.LoadTest.Analysis as Pronk
import qualified Network.HTTP.LoadTest as Pronk
import qualified Network.HTTP.LoadTest.Report as Pronk

import Thentos.Config (ThentosConfig)
import Thentos.Action.Types (ActionState(..))
import Thentos.Transaction
import Thentos.Transaction.Core
import Thentos.Types
    ( UserFormData(UserFormData), UserName(..), UserPass(..), parseUserEmail
    , UserId, ThentosSessionToken(fromThentosSessionToken), UserId(..)
    , ByUserOrServiceId(ByUser)
    )

import Thentos.Test.Core


main :: IO ()
main = do
    withFrontendAndBackend $ \as@(ActionState cfg _ _) -> do
        threadDelay $ let s = (* (1000 * 1000)) in s 2

        Just sessToken <- getThentosSessionToken as
        runSignupBench cfg sessToken
        runLoginBench as
        runCheckTokenBench cfg sessToken


runSignupBench :: ThentosConfig -> ThentosSessionToken -> IO ()
runSignupBench cfg sessionToken = do
    gen <- newStdGen
    let conf = pronkConfig $ mkSignupGens cfg gen sessionToken
    runBench "Signup Benchmark" conf

runLoginBench :: ActionState -> IO ()
runLoginBench as = do
    conf <- pronkConfig `fmap` mkLoginGens as
    runBench "Login Benchmark" conf

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

getThentosSessionToken :: ActionState -> IO (Maybe ThentosSessionToken)
getThentosSessionToken (ActionState cfg _ conn) = do
    let Just godName = UserName <$> cfg >>. (Proxy :: Proxy '["default_user", "name"])
        Just godPass = UserPass <$> cfg >>. (Proxy :: Proxy '["default_user", "password"])
    Right (godUid, _) <- runThentosQuery conn $ lookupConfirmedUserByName godName
    let Just req_ = makeEndpoint cfg "/thentos_session"
        req = req_
                { requestBody = RequestBodyLBS $ Aeson.encode (ByUser godUid godPass)
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

-- | Like 'Data.Aeson.decode' but allows all JSON values instead of just
-- objects and arrays.
--
-- FIXME: upgrade to aeson >= 0.10 and use 'Aeson.eitherDecode' instead of this: See
-- 4b370592242d4e4367ca46d852109c3927210f4b.  for this to work, we need to either upgrade pronk
-- (criterion in particular) benchmarking or, preferably, factor it out into a separate package.
decodeLenient :: Aeson.FromJSON a => LBS -> Either String a
decodeLenient input = do
    v :: Aeson.Value <- AP.parseOnly (Aeson.value <* AP.endOfInput) (cs input)
    Aeson.parseEither Aeson.parseJSON v


-- specific benchmarks

-- captchas

-- | Only works with running thentos-captcha service (see url in the code).  The following figures
-- are from a thinkpad t420s quad-core laptop with logging in thentos-captcha disabled (logging to
-- stdout on DEBUG level made a difference of 10% in both `runpar` tests).
--
-- >>> Benchmark load-test: RUNNING...
-- >>> Captcha Benchmark p/g:  in 29.51 sec (31.2% CPU)
-- >>> latency:
-- >>>     mean:    2.514 sec
-- >>>     std dev: 280.3 msec
-- >>>     99%:     3.199 sec
-- >>>     99.9%:   3.518 sec
-- >>>
-- >>> throughput:  33.89 req/sec
-- >>> Captcha Benchmark s/g:  in 7.814 sec (1.9% CPU)
-- >>> latency:
-- >>>     mean:    77.99 msec
-- >>>     std dev: 79.19 msec
-- >>>     99%:     341.5 msec
-- >>>     99.9%:   678.9 msec
-- >>>
-- >>> throughput:  12.80 req/sec
-- >>> Captcha Benchmark p/a:  in 8.759 sec (77.8% CPU)
-- >>> latency:
-- >>>     mean:    739.1 msec
-- >>>     std dev: 134.4 msec
-- >>>     99%:     997.2 msec
-- >>>     99.9%:   1.123 sec
-- >>>
-- >>> throughput:  114.2 req/sec
-- >>> Captcha Benchmark s/a:  in 1.287 sec (7.8% CPU)
-- >>> latency:
-- >>>     mean:    12.69 msec
-- >>>     std dev: 2.301 msec
-- >>>     99%:     22.49 msec
-- >>>     99.9%:   25.71 msec
-- >>>
-- >>> throughput:  77.68 req/sec
-- >>> Benchmark load-test: FINISH
runCaptchaBench :: IO ()
runCaptchaBench = do
    let frontend = "http://localhost:7002/"
    reqGraphics <- (\r -> r { method = methodPost }) <$> parseUrl (frontend <> "captcha")
    reqAudio <- (\r -> r { method = methodPost }) <$> parseUrl (frontend <> "audio_captcha/en")

    let conf = Pronk.Config {
                      concurrency = 0
                    , numRequests = 0
                    , requestsPerSecond = 0
                    , timeout = 30
                    , requests = []
                    }

        injectReq cnf req = cnf
            { requests = [Pronk.RequestGeneratorConstant $ Pronk.Req req]
            }
        runpar cnf = cnf
            { concurrency = 100
            , numRequests = 1000
            , requestsPerSecond = 1000
            }
        runseq cnf = cnf
            { concurrency = 1
            , numRequests = 100
            , requestsPerSecond = 1000
            }

    runBench "Captcha Benchmark p/g: " . runpar . injectReq conf $ reqGraphics
    runBench "Captcha Benchmark s/g: " . runseq . injectReq conf $ reqGraphics
    runBench "Captcha Benchmark p/a: " . runpar . injectReq conf $ reqAudio
    runBench "Captcha Benchmark s/a: " . runseq . injectReq conf $ reqAudio


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
            , requestBody = RequestBodyLBS (Aeson.encode formData)
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
    loginCont (decodeLenient . responseBody -> Right tok) = LoggedIn tok
    loginCont bad = error $ "loginGenTrans: " ++ show (uid, bad)

    loginReq = (makeRequest cfg Nothing "/thentos_session")
            { method = methodPost
            , requestBody = RequestBodyLBS $ Aeson.encode (ByUser uid "dummyPassword")
            }

    logout tok = (logoutReq tok, const LoggedOut)

    logoutReq tok =
        (makeRequest cfg (Just tok) "/thentos_session")
            { method = methodDelete
            , requestBody = RequestBodyLBS $ Aeson.encode tok
            }

mkLoginGens :: ActionState -> IO [Pronk.RequestGenerator]
mkLoginGens (ActionState cfg _ connPool) = do
    uids <- getUIDs connPool
    -- take out god user so all users have the same password
    uids' <- do
        let Just godName = UserName <$> cfg >>. (Proxy :: Proxy '["default_user", "name"])
        Right (godUid, _) <- runThentosQuery connPool $ lookupConfirmedUserByName godName
        return $ filter (/= godUid) uids
    return . cycle $ map makeGenerator uids'
  where
    makeGenerator :: UserId -> Pronk.RequestGenerator
    makeGenerator uid =
        Pronk.RequestGeneratorStateMachine
            "Login"
            (MachineState uid LoggedOut)
            (loginGenTrans cfg)

getUIDs :: Pool Connection -> IO [UserId]
getUIDs connPool = withResource connPool $
    \conn -> fromOnly <$$> query_ conn "select id from users"


-- checking session tokens

-- This is an intentionally trivial benchmark that is supposed to give
-- us a performance baseline by measuring mostly overhead (routing, json
-- decoding etc.)

runCheckTokenBench :: ThentosConfig -> ThentosSessionToken -> IO ()
runCheckTokenBench cfg sessionToken = do
    let conf = pronkConfig (repeat $ sessionCheckGen cfg sessionToken)
    runBench "Session-check Benchmark" conf

sessionCheckGen :: ThentosConfig -> ThentosSessionToken -> Pronk.RequestGenerator
sessionCheckGen cfg sessionToken = Pronk.RequestGeneratorConstant $ Req req
  where
    req = (makeRequest cfg (Just sessionToken) "/thentos_session")
            {requestBody = RequestBodyLBS $ Aeson.encode sessionToken }
