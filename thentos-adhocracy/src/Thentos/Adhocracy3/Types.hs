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
    , dbA3Users
    , SsoToken(..)
    , ThentosError(..)
    )
    where

import Control.Applicative ((<$>))
import Control.Exception (Exception)
import Control.Lens ((.~), (^.))
import Control.Lens (makeLenses)
import Data.Data (Typeable)
import Data.Map (Map)
import Data.SafeCopy (SafeCopy, deriveSafeCopy, base, putCopy, getCopy)
import Data.Set (Set)
import Data.String.Conversions (LBS, ST)
import Data.Thyme.Time () -- required for NominalDiffTime's num instance
import GHC.Generics (Generic)
import Servant.Server.Internal.ServantErr (err500, errBody)
import System.Log.Logger (Priority(ERROR))

import Thentos.Backend.Core
import Thentos.Types hiding (DB)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Thentos.Action.Core as AC
import qualified Thentos.Types

data DB = DB
    { _dbCoreDB    :: !Thentos.Types.DB
    , _dbSsoTokens :: !(Set SsoToken)
    , _dbA3Users   :: !(Map UserId A3User)
    } deriving (Eq, Show, Typeable, Generic)

instance EmptyDB DB where
    emptyDB = DB emptyDB Set.empty Map.empty

instance DB `Extends` Thentos.Types.DB where
    focus f (DB db ssoTokens a3users) = q <$> f (p db a3users)
      where
        -- move users from the a3 map back into core type.
        q :: Thentos.Types.DB -> DB
        q db' = DB (dbUsers .~ Map.empty $ db') ssoTokens (Map.map extendUser $ db' ^. dbUsers)

        -- move users from the core type back out to the a3 map.
        p :: Thentos.Types.DB -> Map UserId A3User -> Thentos.Types.DB
        p db' a3users' = dbUsers .~ Map.map reduceUser a3users' $ db'

        -- FIXME: github users are not covered by this, but somehow need to be mapped as well, or
        -- things will start to surprise everybody later on.  one way to make this total is to make
        -- the password field `Maybe` in core and make `A3User` a proper extension of `User`.  this
        -- also means that every time we lens from child db into parent db, on the way back we have
        -- to look up every github id from the user in the original child user table before we can
        -- reconstruct the user from the the parent user.
        reduceUser :: A3User -> User
        reduceUser (A3User name (HashedPW pass) email sessions services) = User name pass email sessions services

        extendUser :: User -> A3User
        extendUser (User name pass email sessions services) = A3User name (HashedPW pass) email sessions services

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


data A3Auth = HashedPW !(HashedSecret UserPass) | GithubId !Integer
    deriving (Eq, Show)

data A3User =
    A3User
      { _userName            :: !UserName
      , _userAuth            :: !A3Auth
      , _userEmail           :: !UserEmail
      , _userThentosSessions :: !(Set ThentosSessionToken)
          -- ^ (service sessions are stored in the resp. value in @DB ^. dbSessions@)
      , _userServices        :: !(Map ServiceId ServiceAccount)
          -- ^ services (with session account information)
      }
   deriving (Eq, Show, Typeable, Generic)


instance ThentosErrorToServantErr DB where
    thentosErrorToServantErr (ThentosA3ErrorCore e) = thentosErrorToServantErr e
    thentosErrorToServantErr e@(A3BackendErrorResponse _ _) =
        (Just (ERROR, show e), err500 { errBody = "exception in a3 backend" })

    thentosErrorToServantErr e@(A3BackendInvalidJson _) = do
        (Just (ERROR, show e), err500 { errBody = "exception in a3 backend: received bad json" })
    -- the following shouldn't actually reach servant:
    thentosErrorToServantErr e@SsoErrorUnknownCsrfToken =
        (Just (ERROR, show e), err500 { errBody = "invalid token returned during sso process" })
    thentosErrorToServantErr e@(SsoErrorCouldNotAccessUserInfo _) =
        (Just (ERROR, show e), err500 { errBody = "error accessing user info" })
    thentosErrorToServantErr e@(SsoErrorCouldNotGetAccessToken _) =
        (Just (ERROR, show e), err500 { errBody = "error retrieving access token" })

newtype SsoToken = SsoToken { fromSsoToken :: ST }
    deriving (Eq, Ord, Show, Read, Typeable, Generic)

makeLenses ''DB

$(deriveSafeCopy 0 'base ''SsoToken)
$(deriveSafeCopy 0 'base ''A3User)
$(deriveSafeCopy 0 'base ''A3Auth)
$(deriveSafeCopy 0 'base ''DB)
