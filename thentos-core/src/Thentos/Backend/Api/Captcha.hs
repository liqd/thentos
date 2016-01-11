{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Thentos.Backend.Api.Captcha where

import Control.Lens ((&), (%~), (.~))
import Control.Monad.Except (catchError, throwError)
import Control.Monad (when)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (ST, SBS, cs)
import Data.Void (Void)
import Network.HTTP.Types (methodOptions)
import Network.HTTP.Types.Status (ok200)
import Network.Wai (Application)
import Servant.API (GetHeaders)
import Servant.API.Header (Header)
import Servant.API ((:<|>)((:<|>)), (:>), Post, Capture, ReqBody, JSON)
import Servant.API.ResponseHeaders (Headers, addHeader)
import Servant.Server (HasServer(route), ServerT, Server, serve, enter)
import Servant.Server.Internal (methodRouterHeaders)
import System.Log.Logger (Priority(INFO))

import qualified Servant.Docs as Docs
import qualified Servant.Foreign.Internal as Foreign

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

type CaptchaOptionsHeaders =
    '[ Header "Access-Control-Allow-Origin" ST
     , Header "Access-Control-Expose-Headers" ST
     , Header "Access-Control-Max-Age" ST
     , Header "Access-Control-Allow-Credentials" ST
     , Header "Access-Control-Allow-Methods" ST
     , Header "Access-Control-Allow-Headers" ST
     ]

addCaptchaOptionsHeaders :: a -> Headers CaptchaOptionsHeaders a
addCaptchaOptionsHeaders a = a
     & addHeader "*"
     & addHeader "X-Thentos-Captcha-Id"
     & addHeader "900"
     & addHeader "true"
     & addHeader "POST,GET,DELETE,PUT,OPTIONS"
     & addHeader "Origin, Content-Type, Accept, X-User-Path, X-User-Token"

type CaptchaHeaders = Header "X-Thentos-Captcha-Id" CaptchaId ': CaptchaOptionsHeaders

addCaptchaHeaders :: CaptchaId -> a -> Headers CaptchaHeaders a
addCaptchaHeaders cid = addHeader cid . addCaptchaOptionsHeaders

type ThentosCaptcha = ThentosCaptchaFrontend :<|> ThentosCaptchaBackend

type ThentosCaptchaFrontend =
       "captcha"                             :> Options (Headers CaptchaOptionsHeaders ())
  :<|> "captcha"                             :> Post    '[PNG] (Headers CaptchaHeaders ImageData)
  :<|> "audio_captcha" :> Capture "voice" ST :> Options (Headers CaptchaOptionsHeaders ())
  :<|> "audio_captcha" :> Capture "voice" ST :> Post    '[WAV] (Headers CaptchaHeaders SBS)

type ThentosCaptchaBackend =
       "solve_captcha" :> ReqBody '[JSON] CaptchaSolution :> Post '[JSON] (JsonTop Bool)

thentosCaptcha :: ServerT ThentosCaptcha (Action Void ())
thentosCaptcha = thentosCaptchaFrontend :<|> thentosCaptchaBackend

thentosCaptchaFrontend :: ServerT ThentosCaptchaFrontend (Action Void ())
thentosCaptchaFrontend =
       preflightH
  :<|> captchaImgH
  :<|> (\_ -> preflightH)
  :<|> captchaWavH

thentosCaptchaBackend :: ServerT ThentosCaptchaBackend (Action Void ())
thentosCaptchaBackend = captchaSolveH

preflightH :: Action Void () (Headers CaptchaOptionsHeaders ())
preflightH = pure $ addCaptchaOptionsHeaders ()

captchaImgH :: Action Void () (Headers CaptchaHeaders ImageData)
captchaImgH = (\(cid, img) -> addCaptchaHeaders cid img) <$> makeCaptcha

captchaWavH :: ST -> Action Void () (Headers CaptchaHeaders SBS)
captchaWavH voice = (\(cid, wav) -> addCaptchaHeaders cid wav) <$> makeAudioCaptcha (cs voice)

captchaSolveH :: CaptchaSolution -> Action Void () (JsonTop Bool)
captchaSolveH (CaptchaSolution cid solution) = JsonTop <$> do
    correct <- solveCaptcha cid solution `catchError` h
    when correct $
        deleteCaptcha cid
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


-- * OPTIONS verb

-- FIXME: this should go elsewhere, ideally to servant, but we only need it here for now.
-- FIXME: not sure if the content type list applies here, as body is always empty.

data Options a

instance {-# OVERLAPPABLE #-} ( GetHeaders (Headers h ()) )
        => HasServer (Options (Headers h ())) where
    type ServerT (Options (Headers h ())) m = m (Headers h ())
    route Proxy = methodRouterHeaders methodOptions (Proxy :: Proxy '[JSON]) ok200

instance {-# OVERLAPPABLE #-} Foreign.HasForeign Foreign.NoTypes
        (Options (Headers CaptchaOptionsHeaders ())) where
    type Foreign (Options (Headers CaptchaOptionsHeaders ())) = Foreign.Req
    foreignFor Proxy Proxy req =
        req & Foreign.funcName  %~ ("options" :)
            & Foreign.reqMethod .~ "OPTIONS"

instance Docs.HasDocs (Options (Headers CaptchaOptionsHeaders ())) where
    docsFor _ dat opts = mempty  -- FIXME: be more helpful here?
