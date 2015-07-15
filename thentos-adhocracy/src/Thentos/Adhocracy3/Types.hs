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
    , ThentosError(..)
    )
    where

import Control.Exception (Exception)
import Control.Lens (makeLenses)
import Data.Data (Typeable)
import Data.Functor.Infix ((<$>))
import Data.SafeCopy (SafeCopy, deriveSafeCopy, base, putCopy, getCopy)
import Data.String.Conversions (LBS)
import Data.Thyme.Time () -- required for NominalDiffTime's num instance
import GHC.Generics (Generic)
import Servant.Server.Internal.ServantErr (err500, errBody)
import System.Log.Logger (Priority(ERROR))

import Thentos.Backend.Core
import Thentos.Transaction.TH
import Thentos.Types hiding (DB)

import qualified Thentos.Action.Core as AC
import qualified Thentos.Types
import qualified Thentos.Transaction as T


newtype DB = DB { fromCoreDB :: Thentos.Types.DB }
  deriving (Eq, Show, Typeable, Generic)

instance EmptyDB DB where
    emptyDB = DB emptyDB

instance DB `Extends` Thentos.Types.DB where
    focus f (DB db) = DB <$> f db
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

deriving instance Eq (ThentosError DB)
deriving instance Show (ThentosError DB)
deriving instance Read (ThentosError DB)

instance Exception (ThentosError DB)

deriving instance Show (AC.ActionError DB)

instance SafeCopy (ThentosError DB)
  where
    putCopy = putCopyViaShowRead
    getCopy = getCopyViaShowRead

instance ThentosErrorToServantErr DB where
    thentosErrorToServantErr (ThentosA3ErrorCore e) = thentosErrorToServantErr e
    thentosErrorToServantErr e@(A3BackendErrorResponse _ _) =
        (Just (ERROR, show e), err500 { errBody = "exception in a3 backend" })

    thentosErrorToServantErr e@(A3BackendInvalidJson _) = do
        (Just (ERROR, show e), err500 { errBody = "exception in a3 backend: received bad json" })


makeLenses ''DB

$(deriveSafeCopy 0 'base ''DB)
$(makeThentosAcidicPhase1 ''DB [])
$(makeThentosAcidicPhase2 ''DB [] [''Thentos.Types.DB] ['T.dbEvents])
