{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}

module Thentos.Adhocracy3.Action
    , a3ServiceId
    , activate
    ( addUser
    , externalUrlOfDefaultPersona
    , login
    , makeExternalUrl
    , resetPassword
    , userIdFromPath
    ) where

import Control.Lens ((^.))
import Control.Monad.Except (MonadError, throwError)
import Data.Configifier ((>>.), Tagged(Tagged))
import Data.List (dropWhileEnd, stripPrefix)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (SBS, ST, cs)
import Safe (readMay)
import System.Log (Priority(DEBUG))

import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Text as ST
import qualified URI.ByteString as URI

import Thentos.Adhocracy3.Action.Types
import Thentos.Config
import Thentos.Types

import qualified Thentos.Action as A
import qualified Thentos.Action.Unsafe as U
import qualified Thentos.Adhocracy3.Action.Unsafe as U


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
makeExternalUrl pn = U.createUserInA3'P pn >>= f
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
    reqResult <- U.resetPasswordInA3'P path
    case reqResult of
        RequestSuccess userPath _a3tok -> do
            uid  <- userIdFromPath userPath
            A._changePasswordUnconditionally uid pass
            stok <- A.startThentosSessionByUserId uid pass
            return $ RequestSuccess userPath stok
        RequestError errMsg -> throwError . OtherError $ GenericA3Error errMsg

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


-- * helper actions

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
