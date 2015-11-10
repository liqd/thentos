{-# LANGUAGE OverloadedStrings           #-}

module Thentos.Adhocracy3.Backend.Core
    ( mkSimpleA3Error
    , a3ActionErrorToServantErr
    )
    where

import Data.String.Conversions (ST)
import Servant.Server (ServantErr)
import Servant.Server.Internal.ServantErr (err400, err401, err500, errBody, errHeaders)
import System.Log.Logger (Priority(DEBUG, ERROR, CRITICAL))
import Text.Show.Pretty (ppShow)
import qualified Thentos.Action.Core as AC
import Thentos.Backend.Core

import Data.Aeson (encode)

import Thentos.Adhocracy3.Types

a3ActionErrorToServantErr :: AC.ActionError ThentosA3Error -> IO ServantErr
a3ActionErrorToServantErr e = do
    errorInfoToServantErr mkA3StyleServantErr . actionErrorA3Info a3Info $ e

-- Construct a simple A3-style error wrapping a single error. 'aeName' is set to "thentos" and
-- 'aeLocation' to "body". Useful for cases where all we really have is a description.
mkSimpleA3Error :: ST -> A3Error
mkSimpleA3Error desc = A3Error {aeName = "thentos", aeLocation = "body", aeDescription = desc}

-- | Construct a ServantErr that looks like those reported by the A3 backend.
-- The backend returns a list of errors but we always use a single-element list, as Thentos
-- aborts at the first detected error.
mkA3StyleServantErr :: ServantErr -> A3ErrorMessage -> ServantErr
mkA3StyleServantErr baseErr err = baseErr
    {errBody = encode $ err, errHeaders = [contentTypeJsonHeader]}

mkA3 :: ErrorInfo ST -> ErrorInfo A3ErrorMessage
mkA3 (p, se, msg) = (p, se, A3ErrorMessage [mkSimpleA3Error msg])

actionErrorA3Info :: Show e
                  => (e -> ErrorInfo A3ErrorMessage) -> AC.ActionError e -> ErrorInfo A3ErrorMessage
actionErrorA3Info other = f
  where
    a3Error a b c = A3ErrorMessage [A3Error a b c]

    f e = case e of
        (AC.ActionErrorThentos te) -> g te
        (AC.ActionErrorAnyLabel _) -> mkA3 (Just (DEBUG, ppShow e), err401, "unauthorized")
        (AC.ActionErrorUnknown  _) -> mkA3 (Just (CRITICAL, ppShow e), err500, "internal error")

    -- For errors specifically relevant to the A3 frontend we mirror the A3 backend errors
    -- exactly so that the frontend recognizes them
    g e = case e of
        OtherError ae -> other ae
        BadCredentials -> (Nothing, err400, a3Error
            "password"
            "body"
            "User doesn't exist or password is wrong")
        UserEmailAlreadyExists -> (Nothing, err400, a3Error
            "data.adhocracy_core.sheets.principal.IUserExtended.email"
            "body"
            "The user login email is not unique")
        UserNameAlreadyExists -> (Nothing, err400, a3Error
            "data.adhocracy_core.sheets.principal.IUserBasic.name"
            "body"
            "The user login name is not unique")
        NoSuchPendingUserConfirmation -> (Nothing, err400, a3Error
            "path"
            "body"
            "Unknown or expired activation path")
        NoSuchThentosSession -> (Nothing, err400, a3Error
            "X-User-Token"
            "header"
            "Invalid user token")
        _ -> mkA3 $ thentosErrorInfo (impossible "other error handled above") e

a3Info :: ThentosA3Error -> ErrorInfo A3ErrorMessage
a3Info ae = case ae of
               GenericA3Error errMsg -> (Nothing, err400, errMsg)
               _                     ->  mkA3 $ f ae
  where
    f (GenericA3Error _) = impossible "generic error handled above"
    f e@(A3BackendErrorResponse _ _) =
        (Just (ERROR, show e), err500, "exception in a3 backend")
    f e@(A3BackendInvalidJson _) =
        (Just (ERROR, show e), err500, "exception in a3 backend: received bad json")
    f e@(A3UriParseError _) =
        (Just (ERROR, show e), err500, "exception in a3 backend: received unparsable URL")
    f e@(A3NoDefaultPersona _ _) =
        (Just (ERROR, show e), err500, "no default persona found for user")
    f e@A3PersonaLacksExternalUrl =
        (Just (ERROR, show e), err500, "no external URL stored for persona")

impossible :: String -> a
impossible = error
