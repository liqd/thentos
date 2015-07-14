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
import Thentos.Types hiding (DB)

import qualified Thentos.Action.Core as AC
import qualified Thentos.Types


newtype DB = DB { fromCoreDB :: Thentos.Types.DB }
  deriving (Eq, Show, Typeable, Generic)

instance EmptyDB DB where
    emptyDB = DB $ Thentos.Types.emptyDB

instance DB `Thentos.Types.Extends` Thentos.Types.DB where
    focus f (DB db) = DB <$> f db
    thentosErrorFromParent = ThentosA3ErrorCore
    thentosErrorToParent (ThentosA3ErrorCore e) = Just e
    thentosErrorToParent _ = Nothing

data instance Thentos.Types.ThentosError DB =
      ThentosA3ErrorCore (Thentos.Types.ThentosError (Thentos.Types.DB))
    | A3BackendErrorResponse Int LBS
    | A3BackendInvalidJson String

deriving instance Eq (Thentos.Types.ThentosError DB)
deriving instance Show (Thentos.Types.ThentosError DB)
deriving instance Read (Thentos.Types.ThentosError DB)

instance Exception (Thentos.Types.ThentosError DB)

deriving instance Show (AC.ActionError DB)

instance SafeCopy (Thentos.Types.ThentosError DB)
  where
    putCopy = Thentos.Types.putCopyViaShowRead
    getCopy = Thentos.Types.getCopyViaShowRead

instance ThentosErrorToServantErr DB where
    thentosErrorToServantErr (ThentosA3ErrorCore e) = thentosErrorToServantErr e
    thentosErrorToServantErr e@(A3BackendErrorResponse _ _) =
        (Just (ERROR, show e), err500 { errBody = "exception in a3 backend" })

    thentosErrorToServantErr e@(A3BackendInvalidJson _) = do
        (Just (ERROR, show e), err500 { errBody = "exception in a3 backend: received bad json" })


makeLenses ''DB

$(deriveSafeCopy 0 'base ''DB)
