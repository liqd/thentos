{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This is an implementation of
-- git@github.com:liqd/adhocracy3.git:/docs/source/api/authentication_api.rst
module Thentos.Adhocracy3.Backend.Api.Simple
    ( A3Resource(..)
    , A3UserNoPass(..)
    , A3UserWithPass(..)
    , ActivationRequest(..)
    , Api
    , ContentType(..)
    , LoginRequest(..)
    , PasswordResetRequest(..)
    , Path(..)
    , RequestResult(..)
    , ThentosApi
    , TypedPath(..)
    , TypedPathWithCacheControl(..)
    , a3corsPolicy
    , a3ProxyAdapter
    , runBackend
    , serveApi
    , thentosApi
    ) where

import Control.Lens ((^.), (&), (<>~))
import Control.Monad.Except (MonadError, throwError)
import Control.Monad (when)
import Data.Aeson (ToJSON)
import Data.CaseInsensitive (mk)
import Data.Configifier ((>>.), Tagged(Tagged))
import Data.List (dropWhileEnd, stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (LBS, SBS, ST, cs)
import LIO.Core (liftLIO)
import LIO.TCB (ioTCB)
import Network.Socket (SockAddr(SockAddrInet))
import Network.Wai (Application)
import Safe (readMay)
import Servant.API ((:<|>)((:<|>)), (:>), Capture, ReqBody, JSON, Post)
import Servant.API.Header (Header)
import Servant.API.ResponseHeaders (Headers, addHeader)
import Servant.Docs (ToSample(toSamples))
import Servant.Server.Internal (Server, ServerT)
import Servant.Server (serve, enter)
import System.Log (Priority(DEBUG, INFO))

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Text as ST
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types.Status as Status
import qualified Servant.Docs as Docs
import qualified URI.ByteString as URI

import System.Log.Missing
import Thentos.Adhocracy3.Action.Types
import Thentos.Adhocracy3.Backend.Core
import Thentos.Backend.Api.Auth.Types
import Thentos.Backend.Api.Docs.Common
    ( RestDocs, restDocs
    , HasDocExtras(getCabalPackageName, getCabalPackageVersion, getTitle, getIntros, getExtraInfo)
    )
import Thentos.Backend.Api.Docs.Proxy ()
import Thentos.Backend.Api.Proxy
import Thentos.Backend.Core
import Thentos.Config
import Thentos.Ends.Types (PNG, WAV)
import Thentos.Types
import Thentos.Util

import qualified Paths_thentos_adhocracy__ as Paths
import qualified Thentos.Action as A
import qualified Thentos.Action.Types as AC
import qualified Thentos.Action.Unsafe as U
import qualified Thentos.Backend.Api.Purescript
import qualified Thentos.Backend.Api.Simple ()


-- * main

runBackend :: HttpConfig -> AC.ActionState -> IO ()
runBackend cfg asg = do
    logger INFO $ "running rest api (a3 style) on " ++ show (bindUrl cfg) ++ "."
    manager <- Client.newManager Client.defaultManagerSettings
    runWarpWithCfg cfg $ serveApi manager cfg asg

serveApi :: Client.Manager -> HttpConfig -> AC.ActionState -> Application
serveApi manager beConfig astate = addCorsHeaders a3corsPolicy . addCacheControlHeaders $
    let p = Proxy :: Proxy (RestDocs Api)
    in serve p (restDocs beConfig p :<|> api manager astate)


-- * api

-- | Note: login_username and login_email have identical behavior.  In
-- particular, it is not an error to send username and password to
-- @/login_email@.  This makes implementing all sides of the protocol
-- a lot easier without sacrificing security.
type ThentosApi =
       "principals" :> "users" :> ReqBody '[JSON] A3UserWithPass
                               :> Post200 '[JSON] TypedPathWithCacheControl
  :<|> "activate_account"      :> ReqBody '[JSON] ActivationRequest
                               :> Post200 '[JSON] RequestResult
  :<|> "login_username"        :> ReqBody '[JSON] LoginRequest
                               :> Post200 '[JSON] RequestResult
  :<|> "login_email"           :> ReqBody '[JSON] LoginRequest
                               :> Post200 '[JSON] RequestResult
  :<|> "password_reset"        :> ReqBody '[JSON] PasswordResetRequest
                               :> Post200 '[JSON] RequestResult
  :<|> "thentos" :> "user" :> ThentosApiWithWidgets

type ThentosApiWithWidgets =
       "register" :> ReqBody '[JSON] UserCreationRequest :> Post '[JSON] ()
  :<|> "activate" :> ReqBody '[JSON] (JsonTop ConfirmationToken)
                  :> Post '[JSON] (JsonTop ThentosSessionToken)
  :<|> "captcha"  :> Post '[PNG] (Headers '[Header "Thentos-Captcha-Id" CaptchaId] ImageData)
  :<|> "audio_captcha" :> Capture "voice" ST
          :> Post '[WAV] (Headers '[Header "Thentos-Captcha-Id" CaptchaId] SBS)

type Api =
       ThentosApi
  :<|> "js" :> Thentos.Backend.Api.Purescript.Api
  :<|> ServiceProxy

-- | thentos-adhocracy's "Simple" api does not use thentos-core's "Auth" functionality, which is
-- baked into the 'Action' monad.  'emptyCreds' is a credential value that can be passed into
-- 'enter' that won't raise the clearance level for anybody.
--
-- FIXME: there really should be a better structure for all this.  either "Auth" is too specific to
-- be baked into 'Action', or it should be general enough to be used here (see comment on
-- 'renderThentosHeaderName').  (Also consider replacing "Auth" with
-- https://github.com/haskell-servant/servant/pull/185 once it's ready.)
emptyCreds :: ThentosAuthCredentials
emptyCreds = ThentosAuthCredentials Nothing (SockAddrInet 0 0)

thentosApi :: AC.ActionState -> Server ThentosApi
thentosApi as = enter (enterAction () as a3ActionErrorToServantErr emptyCreds) $
       addUser
  :<|> activate
  :<|> login
  :<|> login
  :<|> resetPassword
  :<|> thentosApiWithWidgets

thentosApiWithWidgets :: ServerT ThentosApiWithWidgets A3Action
thentosApiWithWidgets =
       A.addUnconfirmedUserWithCaptcha
  :<|> (JsonTop <$>) . (snd <$>) .  A.confirmNewUser . fromJsonTop
  :<|> (A.makeCaptcha >>= \(cid, img) -> return $ addHeader cid img)
  :<|> (\voice -> A.makeAudioCaptcha (cs voice) >>= \(cid, wav) -> return $ addHeader cid wav)


api :: Client.Manager -> AC.ActionState -> Server Api
api manager actionState@(AC.ActionState (_, _, cfg)) =
       thentosApi actionState
  :<|> Thentos.Backend.Api.Purescript.api cfg
  :<|> serviceProxy manager a3ProxyAdapter actionState


-- * handler

-- | Add a user in Thentos, but not yet in A3. Only after the Thentos user has been activated
-- (confirmed), a persona is created in thentos together with a corresponding adhocracy user in A3
-- that corresponds to that persona.
addUser :: A3UserWithPass -> A3Action TypedPathWithCacheControl
addUser (A3UserWithPass user) = U.logIfError' $ do
    U.unsafeAction . U.logger DEBUG . ("route addUser: " <>) . cs .
        Aeson.encodePretty $ A3UserNoPass user
    A.addUnconfirmedUser user
    config <- U.unsafeAction U.getConfig
    let dummyPath = a3backendPath config ""
    return $ TypedPathWithCacheControl (TypedPath dummyPath CTUser) [] [] [] []

    -- FIXME Which path should we return here, considering that no A3 user exists yet?!

    -- FIXME We correctly return empty cache-control info since the A3 DB isn't (yet) changed,
    -- but normally the A3 backend would return the following info if a new user is created:
    -- changedDescendants: "", "principals/", "principals/users/", "principals/groups/"
    -- created: userPath
    -- modified: "principals/users/", "principals/groups/authenticated/"
    -- Find out if not returning this stuff leads to problems in the A3 frontend!
    --
    -- possible solution: deliver thentos registration widget; disable all adhocracy frontend-code
    -- that touches this end-point; provide user resources from outside of widgets only.

-- | Activate a new user. This also creates a persona and a corresponding adhocracy user in the A3 backend,
-- so that the user is able to log into A3. The user's actual password and email address are
-- only stored in Thentos and NOT exposed to A3.
activate :: ActivationRequest -> A3Action RequestResult
activate ar@(ActivationRequest confToken) = U.logIfError' $ do
    U.unsafeAction . U.logger DEBUG . ("route activate:" <>) . cs $ Aeson.encodePretty ar
    (uid, stok) <- A.confirmNewUser confToken
    -- Promote access rights so we can look up the user and create a persona
    U.extendClearanceOnAgent (UserA uid)
    user <- snd <$> A.lookupConfirmedUser uid
    let persName = PersonaName . fromUserName $ user ^. userName
    externalUrl <- makeExternalUrl persName
    persona     <- A.addPersona persName uid $ Just externalUrl
    sid         <- a3ServiceId
    -- Register persona for the default ("") context of the default service (A3)
    A.registerPersonaWithContext persona sid ""
    pure $ RequestSuccess (Path . cs . renderUri $ externalUrl) stok

-- | Make user path relative to our exposed URL instead of the proxied A3 backend URL.  Only works
-- for @/principals/users/...@.  (Returns exposed url.)
makeExternalUrl :: PersonaName -> A3Action Uri
makeExternalUrl pn = createUserInA3'P pn >>= f
  where
    f :: Path -> A3Action Uri
    f (Path path@(ST.breakOn "/principals/users/" -> (_, localPath)))
        | ST.null localPath = do
            throwError . OtherError . A3UriParseError . URI.OtherError $ "bad A3 user uri: " <> cs path
        | otherwise = do
            config <- U.unsafeAction U.getConfig
            let (Path fullPath) = a3backendPath config localPath
            case parseUri $ cs fullPath  of
                Left err  -> throwError . OtherError $ A3UriParseError err
                Right uri -> pure uri

-- | Log a user in.
login :: LoginRequest -> A3Action RequestResult
login r = U.logIfError' $ do
    U.unsafeAction $ U.logger DEBUG "/login/"
    (uid, stok) <- case r of
        LoginByName  uname pass -> A.startThentosSessionByUserName uname pass
        LoginByEmail email pass -> A.startThentosSessionByUserEmail email pass
    userUrl <- externalUrlOfDefaultPersona uid
    return $ RequestSuccess (Path $ cs userUrl) stok

-- | Allow a user to reset their password. This endpoint is called by the A3 frontend after the user
-- has clicked on the link in a reset-password mail sent by the A3 backend. To check whether the
-- reset path is valid, we forward the request to the backend, but replacing the new password by a
-- dummy (as usual). If the backend indicates success, we update the password in Thentos.
-- A successful password reset will activate not-yet-activated users, as per the A3 API spec.
-- BUG #321: Process is now broken, adapt to new user management (user is now stored in
-- Thentos with a corresponding persona in A3 for activated users only.)
resetPassword :: PasswordResetRequest -> A3Action RequestResult
resetPassword (PasswordResetRequest path pass) = U.logIfError' $ do
    U.unsafeAction . U.logger DEBUG $ "route password_reset for path: " <> show path
    reqResult <- resetPasswordInA3'P path
    case reqResult of
        RequestSuccess userPath _a3tok -> do
            uid  <- userIdFromPath userPath
            A._changePasswordUnconditionally uid pass
            stok <- A.startThentosSessionByUserId uid pass
            return $ RequestSuccess userPath stok
        RequestError errMsg -> throwError . OtherError $ GenericA3Error errMsg


-- * helper actions

-- | Create a user in A3 from a persona name and return the user path.
createUserInA3'P :: PersonaName -> A3Action Path
createUserInA3'P persName = do
    config <- U.unsafeAction U.getConfig
    let a3req = fromMaybe
                (error "createUserInA3'P: mkUserCreationRequestForA3 failed, check config!") $
                mkUserCreationRequestForA3 config persName
    a3resp <- liftLIO . ioTCB . sendRequest $ a3req
    when (responseCode a3resp >= 400) $ do
        throwError . OtherError . A3BackendErrorResponse (responseCode a3resp) $
            Client.responseBody a3resp
    extractUserPath a3resp
  where
    responseCode = Status.statusCode . Client.responseStatus

-- | Find the ServiceId of the A3 backend, which should be registered as default proxied app.
a3ServiceId :: A3Action ServiceId
a3ServiceId = do
    config  <- U.unsafeAction U.getConfig
    maybe (error "a3ServiceId: A3 proxy not configured") return $
        ServiceId <$> config >>. (Proxy :: Proxy '["proxy", "service_id"])

-- | Return the external URL of a user's default ("") persona, in rendered form.
externalUrlOfDefaultPersona :: UserId -> A3Action SBS
externalUrlOfDefaultPersona uid = do
    sid     <- a3ServiceId
    persona <- A.findPersona uid sid "" >>=
               maybe (throwError . OtherError $ A3NoDefaultPersona uid sid) pure
    userUrl <- maybe (throwError $ OtherError A3PersonaLacksExternalUrl) pure $
                     persona ^. personaExternalUrl
    pure $ renderUri userUrl

-- | Send a password reset request to A3 and return the response.
resetPasswordInA3'P :: Path -> A3Action RequestResult
resetPasswordInA3'P path = do
    config <- U.unsafeAction U.getConfig
    let a3req = fromMaybe (error "resetPasswordInA3'P: mkRequestForA3 failed, check config!") $
                mkRequestForA3 config "/password_reset" reqData
    a3resp <- liftLIO . ioTCB . sendRequest $ a3req
    either (throwError . OtherError . A3BackendInvalidJson) return $
        (Aeson.eitherDecode . Client.responseBody $ a3resp :: Either String RequestResult)
  where
    reqData = PasswordResetRequest path "dummypass"


-- * policy

a3corsPolicy :: CorsPolicy
a3corsPolicy = CorsPolicy
    { corsHeaders = "Origin, Content-Type, Accept, X-User-Path, X-User-Token"
    , corsMethods = "POST,GET,DELETE,PUT,OPTIONS"
    , corsOrigin  =  "*"
    }


-- * low-level helpers

-- | Convert a persona name into a user creation request to be sent to the A3 backend.
-- The persona name is used as user name. The email address is set to a unique dummy value
-- and the password is set to a dummy value.
mkUserCreationRequestForA3 :: ThentosConfig -> PersonaName -> Maybe Client.Request
mkUserCreationRequestForA3 config persName = do
    let user  = UserFormData { udName     = UserName $ fromPersonaName persName,
                               udEmail    = email,
                               udPassword = "dummypass" }
    mkRequestForA3 config "/principals/users" $ A3UserWithPass user
  where
    rawEmail = cs (mailEncode $ fromPersonaName persName) <> "@example.org"
    email    = fromMaybe (error $ "mkUserCreationRequestForA3: couldn't create dummy email") $
                         parseUserEmail rawEmail

-- | Make a POST request to be sent to the A3 backend. Returns 'Nothing' if the 'ThentosConfig'
-- lacks a correctly configured proxy.
--
-- Note that the request is configured to NOT thrown an exception even if the response status code
-- indicates an error (400 or higher). Properly dealing with error replies is left to the caller.
--
-- Since the A3 frontend doesn't know about different services (i.e. never sends a
-- @X-Thentos-Service@ header), we send the request to the default proxy which should be the A3
-- backend.
mkRequestForA3 :: ToJSON a => ThentosConfig -> String -> a -> Maybe Client.Request
mkRequestForA3 config route dat = do
    defaultProxy <- Tagged <$> config >>. (Proxy :: Proxy '["proxy"])
    let target = extractTargetUrl defaultProxy
    initReq <- Client.parseUrl $ cs $ show target <//> route
    return initReq { Client.method = "POST"
        , Client.requestHeaders = [("Content-Type", "application/json")]
        , Client.requestBody = Client.RequestBodyLBS . Aeson.encode $ dat
        , Client.checkStatus = \_ _ _ -> Nothing
        }

-- | Extract the user path from an A3 response received for a user creation request.
-- FIXME: make use of servant-client for all rest communication with A3 backend!
extractUserPath :: (MonadError (ThentosError ThentosA3Error) m) => Client.Response LBS -> m Path
extractUserPath resp = do
    resource <- either (throwError . OtherError . A3BackendInvalidJson) return $
        (Aeson.eitherDecode . Client.responseBody $ resp :: Either String TypedPath)
    pure $ tpPath resource

sendRequest :: Client.Request -> IO (Client.Response LBS)
sendRequest req = Client.newManager Client.defaultManagerSettings >>= Client.httpLbs req

-- | A3-specific ProxyAdapter.
a3ProxyAdapter :: ProxyAdapter ThentosA3Error
a3ProxyAdapter = ProxyAdapter
  { renderHeader = renderA3HeaderName
  , renderUser   = a3RenderUser
  , renderError  = a3ActionErrorToServantErr
  }

-- | Render Thentos/A3-specific custom headers using the names expected by A3.
renderA3HeaderName :: RenderHeaderFun
renderA3HeaderName ThentosHeaderSession = mk "X-User-Token"
renderA3HeaderName ThentosHeaderUser    = mk "X-User-Path"
renderA3HeaderName h                    = renderThentosHeaderName h

-- | Render the user as A3 expects it. We return the external URL of the user's default persona.
a3RenderUser :: UserId -> User -> A3Action SBS
a3RenderUser uid _ = externalUrlOfDefaultPersona uid

-- | Convert a local file name into a absolute path relative to the A3 backend endpoint.  (Returns
-- exposed url.)
a3backendPath :: ThentosConfig -> ST -> Path
a3backendPath config localPath = Path $ cs (exposeUrl beHttp) <//> localPath
  where
    beHttp     = case config >>. (Proxy :: Proxy '["backend"]) of
                     Nothing -> error "a3backendPath: backend not configured!"
                     Just v -> Tagged v

userIdFromPath :: MonadError (ThentosError e) m => Path -> m UserId
userIdFromPath (Path s) = do
    uri <- either (const . throwError . MalformedUserPath $ s) return $
        URI.parseURI URI.laxURIParserOptions $ cs s
    rawId <- maybe (throwError $ MalformedUserPath s) return $
        stripPrefix "/principals/users/" $ dropWhileEnd (== '/') (cs $ URI.uriPath uri)
    maybe (throwError NoSuchUser) (return . UserId) $ readMay rawId


-- * servant docs

instance HasDocExtras (RestDocs Api) where
    getCabalPackageName _ = "thentos-adhocracy"
    getCabalPackageVersion _ = Paths.version
    getTitle _ = "The thentos API family: Adhocracy3 Proxy"
    getIntros _ =
        [ Docs.DocIntro "@@0.2@@Overview" [unlines $
            [ "Adhocracy3 has a basic user management built-in.  In order for thentos"
            , "to have minimal impact on the existing code base, it can be deployed"
            , "as a reverse proxy and mimic the built-in user management rest api."
            , "This way, the frontend does not need to change at all to use the old"
            , "features of the new user management system.  The impact of new"
            , "features to the frontend can be kept at a minimum."
            , ""
            , "What follows is the fully compatible adhocracy3 user management rest"
            , "api.  Any deviation should be considered an error and reported in a"
            , "later version of this document."
            ]]]

    getExtraInfo _ = mconcat
        [ Docs.extraInfo (Proxy :: Proxy ("principals" :> "users" :> ReqBody '[JSON] A3UserWithPass
                               :> Post200 '[JSON] TypedPathWithCacheControl))
                $ Docs.defAction & Docs.notes <>~
            [ Docs.DocNote "request creation of a new account" [unlines $
                [ "When the user-creation form is filled out with login name, email, and"
                , "password, this end-point is used to post the form content and trigger"
                , "the email confirmation procedure."
                ]
            ]]
        , Docs.extraInfo (Proxy :: Proxy ("activate_account" :> ReqBody '[JSON] ActivationRequest
                               :> Post200 '[JSON] RequestResult))
                $ Docs.defAction & Docs.notes <>~
            [ Docs.DocNote "email-click account activation" [unlines $
                [ "The confirmation email contains a link to this end-point."
                , "The path contains a token can only be learned from receiving"
                , "(or intercepting) the email."
                ]
            ]]
        , Docs.extraInfo (Proxy :: Proxy ("login_username" :> ReqBody '[JSON] LoginRequest
                               :> Post200 '[JSON] RequestResult))
                $ Docs.defAction & Docs.notes <>~
            [ Docs.DocNote "login with user name" []
            ]
        , Docs.extraInfo (Proxy :: Proxy ("login_email" :> ReqBody '[JSON] LoginRequest
                               :> Post200 '[JSON] RequestResult))
                $ Docs.defAction & Docs.notes <>~
            [ Docs.DocNote "login with user email" []
            ]
        , Docs.extraInfo (Proxy :: Proxy ("password_reset" :> ReqBody '[JSON] PasswordResetRequest
                               :> Post200 '[JSON] RequestResult))
                $ Docs.defAction & Docs.notes <>~
            [ Docs.DocNote "reset password" []
            ]
        ]

instance ToSample A3UserNoPass

instance ToSample A3UserWithPass

instance ToSample a => ToSample (A3Resource a)

instance ToSample TypedPath

instance ToSample Path where
    toSamples _ = Docs.singleSample $ Path "/proposals/environment"

instance ToSample TypedPathWithCacheControl

instance ToSample ActivationRequest

-- FIXME: split up LoginRequest into two separate types for login by email
-- and login by user name, in order to provide a correct example for
-- login_email request body
instance ToSample LoginRequest

instance ToSample RequestResult where
    toSamples _ = [ ("Success", RequestSuccess (Path "somepath") "sometoken")]

instance ToSample PasswordResetRequest

instance ToSample ContentType where
    toSamples _ = Docs.singleSample CTUser
