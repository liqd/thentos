{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE TypeOperators                            #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Thentos.Backend.Api.Captcha where

import Control.Monad (when)
import Control.Monad.Except (catchError, throwError)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (ST, SBS, cs)
import Data.Void (Void)
import Servant.API.Header (Header)
import Network.Wai (Application)
import Servant.API ((:<|>)((:<|>)), (:>), Post, Capture, ReqBody, JSON)
import Servant.Server (ServerT, Server, serve, enter)
import Servant.API.ResponseHeaders (Headers, addHeader)
import System.Log.Logger (Priority(INFO))

import qualified Servant.Docs as Docs

import System.Log.Missing (logger)
import Thentos.Action
import Thentos.Action.Types (ActionState, Action)
import Thentos.Backend.Api.Auth
import Thentos.Backend.Api.Docs.Common
import Thentos.Backend.Core
import Thentos.Config
import Thentos.Ends.Types
import Thentos.Types

import qualified Paths_thentos_core__ as Paths


-- * main

runApi :: HttpConfig -> ActionState -> IO ()
runApi cfg asg = do
    logger INFO $ "running rest api Thentos.Backend.Api.Captcha on " ++ show (bindUrl cfg) ++ "."
    runWarpWithCfg cfg $ serveApi cfg asg

serveApi :: HttpConfig -> ActionState -> Application
serveApi cfg astate = addCacheControlHeaders $
    let p = Proxy :: Proxy (RestDocs Api)
    in serve p (restDocs cfg p :<|> api astate)

type Api = ThentosAuth :> ThentosCaptcha

api :: ActionState -> Server Api
api as = \creds -> enter (enterAction () as baseActionErrorToServantErr creds) thentosCaptcha


-- * captcha

type CaptchaHeaders a = Headers '[Header "Thentos-Captcha-Id" CaptchaId] a

type ThentosCaptcha =
       "captcha"                             :> Post '[PNG] (CaptchaHeaders ImageData)
  :<|> "audio_captcha" :> Capture "voice" ST :> Post '[WAV] (CaptchaHeaders SBS)
  :<|> "solve_captcha" :>  ReqBody '[JSON] CaptchaSolution :> Post '[JSON] (JsonTop Bool)

thentosCaptcha :: ServerT ThentosCaptcha (Action Void ())
thentosCaptcha =
       captchaImgH
  :<|> captchaWavH
  :<|> captchaSolveH

captchaImgH :: Action Void () (CaptchaHeaders ImageData)
captchaImgH = (\(cid, img) -> addHeader cid img) <$> makeCaptcha

captchaWavH :: ST -> Action Void () (CaptchaHeaders SBS)
captchaWavH voice = (\(cid, wav) -> addHeader cid wav) <$> makeAudioCaptcha (cs voice)

captchaSolveH :: CaptchaSolution -> Action Void () (JsonTop Bool)
captchaSolveH (CaptchaSolution cId solution) = JsonTop <$> do
    correct <- solveCaptcha cId solution `catchError` h
    when correct $
        deleteCaptcha cId
    return correct
  where
    h NoSuchCaptchaId = return False
    h e               = throwError e

-- * servant docs

instance HasDocExtras (RestDocs Api) where
    getCabalPackageName _ = "thentos-core"
    getCabalPackageVersion _ = Paths.version

    getTitle _ = "The thentos API family: Captcha"

    getIntros _ =
        [ Docs.DocIntro "@@0.2@@Overview" [unlines $
            [ "A lean service that provides visual and audio captchas and verifies whether"
            , "submitted solutions are correct."
            ]]]
