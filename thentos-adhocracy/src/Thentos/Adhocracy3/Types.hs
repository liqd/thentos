{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveFunctor               #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE KindSignatures              #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE UndecidableInstances        #-}

module Thentos.Adhocracy3.Types
    ( module Core
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

import Thentos.Types as Core hiding (DB)
import qualified Thentos.Types as Core


newtype DB = DB { fromCoreDB :: Core.DB }
  deriving (Eq, Show, Typeable, Generic)

instance EmptyDB DB where
    emptyDB = DB $ Core.emptyDB

instance DB `Core.Extends` Core.DB where
    focus f (DB db) = DB <$> f db
    asDBThentosError = ThentosA3ErrorCore

data instance Core.ThentosError DB =
      ThentosA3ErrorCore (Core.ThentosError (Core.DB))
    | A3BackendErrorResponse Int LBS
    | A3BackendInvalidJson String

deriving instance Eq (Core.ThentosError DB)
deriving instance Show (Core.ThentosError DB)
deriving instance Read (Core.ThentosError DB)

instance Exception (Core.ThentosError DB)

instance SafeCopy (Core.ThentosError DB)
  where
    putCopy = Core.putCopyViaShowRead
    getCopy = Core.getCopyViaShowRead


-- * boilerplate

makeLenses ''DB

$(deriveSafeCopy 0 'base ''DB)
