{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE UndecidableInstances        #-}


module Thentos.Adhocracy3.Types
    ( module Thentos.Types
    , DB(..)
    , dbCoreDB
    , dbSsoTokens
    , SsoToken(..)
    , ThentosError(..)
    )
    where

import Control.Applicative ((<$>))
import Control.Exception (Exception)
import Control.Lens (makeLenses)
import Data.Aeson (Value(String), ToJSON(toJSON), (.=), encode, object)
import Data.Data (Typeable)
import Data.Maybe (fromMaybe)
import Data.SafeCopy (SafeCopy, deriveSafeCopy, base, putCopy, getCopy)
import Data.Set (Set)
import Data.String.Conversions (LBS, ST)
import Data.Thyme.Time () -- required for NominalDiffTime's num instance
import GHC.Generics (Generic)
import Servant.Server (ServantErr)
import Servant.Server.Internal.ServantErr (err400, err500, errBody, errHeaders)
import System.Log.Logger (Priority(ERROR))

import Thentos.Backend.Core
import Thentos.Types hiding (DB)

import qualified Data.Set as Set
import qualified Thentos.Action.Core as AC
import qualified Thentos.Types

data DB = DB
    { _dbCoreDB    :: !Thentos.Types.DB
    , _dbSsoTokens :: !(Set SsoToken)
    } deriving (Eq, Show, Typeable, Generic)

instance EmptyDB DB where
    emptyDB = DB emptyDB Set.empty

instance DB `Extends` Thentos.Types.DB where
    focus f (DB db ssoTokens) = flip DB ssoTokens <$> f db
    thentosErrorFromParent = ThentosA3ErrorCore
    thentosErrorToParent (ThentosA3ErrorCore e) = Just e
    thentosErrorToParent _ = Nothing

instance DB `Extends` DB where
    focus = id
    thentosErrorFromParent = id
    thentosErrorToParent = Just

data instance ThentosError DB =
      ThentosA3ErrorCore (ThentosError Thentos.Types.DB)
    | A3BackendErrorResponse Int LBS
    | A3BackendInvalidJson String
    | SsoErrorUnknownCsrfToken
    | SsoErrorCouldNotAccessUserInfo LBS
    | SsoErrorCouldNotGetAccessToken LBS

deriving instance Eq (ThentosError DB)
deriving instance Show (ThentosError DB)
deriving instance Read (ThentosError DB)

instance Exception (ThentosError DB)

deriving instance Show (AC.ActionError DB)

instance SafeCopy (ThentosError DB)
  where
    putCopy = putCopyViaShowRead
    getCopy = getCopyViaShowRead

data A3ErrorInfo = A3ErrorInfo
    { aeName        :: ST
    , aeLocation    :: ST
    , aeDescription :: ST
    } deriving (Eq, Show)

instance ToJSON A3ErrorInfo where
    toJSON err = object
        [ "status" .= String "error"
        , "errors" .= [ object
            [ "name"        .= aeName err
            , "location"    .= aeLocation err
            , "description" .= aeDescription err
            ]]
        ]

-- Construct a simple A3-style error, with 'aeName' set to "thentos" and 'aeLocation' set to "body".
-- Useful for cases where all we really have is a description.
mkSimpleA3ErrorInfo :: ST -> A3ErrorInfo
mkSimpleA3ErrorInfo desc = A3ErrorInfo
    {aeName = "thentos", aeLocation = "body", aeDescription = desc}

-- | Construct a ServantErr that looks like those reponrted by the A3 backend.
-- The backend returns a list of errors but we always use a single-element list, as Thentos
-- aborts at the first detected error.
mkA3StyleServantErr :: ServantErr -> A3ErrorInfo -> ServantErr
mkA3StyleServantErr baseErr errorInfo = baseErr
    {errBody = encode errorInfo, errHeaders = [contentTypeJsonHeader]}

-- | Construct a simple A3-style 'ServantErr' from only a base error and a message, setting the
-- other A3-specific fields to default values.
mkSimpleA3StyleServantErr :: MkServantErrFun
mkSimpleA3StyleServantErr baseErr msg = mkA3StyleServantErr baseErr $ mkSimpleA3ErrorInfo msg

instance ThentosErrorToServantErr DB where
    thentosErrorToServantErr mMkErr = f
      where
        mkErr = fromMaybe mkSimpleA3StyleServantErr mMkErr

        -- For errors specifically relevant to the A3 frontend we mirror the A3 backend errors
        -- exactly so that the frontend recognizes them
        f (ThentosA3ErrorCore e) = case e of
            BadCredentials -> (Nothing, mkA3StyleServantErr err400 $ A3ErrorInfo
                "password"
                "body"
                "User doesn't exist or password is wrong")
            UserEmailAlreadyExists -> (Nothing, mkA3StyleServantErr err400 $ A3ErrorInfo
                "data.adhocracy_core.sheets.principal.IUserExtended.email"
                "body"
                "The user login email is not unique")
            UserNameAlreadyExists -> (Nothing, mkA3StyleServantErr err400 $ A3ErrorInfo
                "data.adhocracy_core.sheets.principal.IUserBasic.name"
                "body"
                "The user login name is not unique")
            NoSuchPendingUserConfirmation -> (Nothing, mkA3StyleServantErr err400 $ A3ErrorInfo
                "path"
                "body"
                "Unknown or expired activation path")
            NoSuchThentosSession -> (Nothing, mkA3StyleServantErr err400 $ A3ErrorInfo
                "X-User-Token"
                "header"
                "Invalid user token")
            _ -> thentosErrorToServantErr (Just mkErr) e

        f e@(A3BackendErrorResponse _ _) =
            (Just (ERROR, show e), mkErr err500 "exception in a3 backend")
        f e@(A3BackendInvalidJson _) =
            (Just (ERROR, show e), mkErr err500 "exception in a3 backend: received bad json")
        -- the following shouldn't actually reach servant:
        f e@SsoErrorUnknownCsrfToken =
            (Just (ERROR, show e), mkErr err500 "invalid token returned during sso process")
        f e@(SsoErrorCouldNotAccessUserInfo _) =
            (Just (ERROR, show e), mkErr err500 "error accessing user info")
        f e@(SsoErrorCouldNotGetAccessToken _) =
            (Just (ERROR, show e), mkErr err500 "error retrieving access token")

newtype SsoToken = SsoToken { fromSsoToken :: ST }
    deriving (Eq, Ord, Show, Read, Typeable, Generic)

makeLenses ''DB

$(deriveSafeCopy 0 'base ''SsoToken)
$(deriveSafeCopy 0 'base ''DB)
