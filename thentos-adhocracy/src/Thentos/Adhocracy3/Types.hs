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
  ( module Thentos.Types
  , module Thentos.Adhocracy3.Types
  )
  where

import Control.Monad.Except (MonadError, throwError)
import Data.Data (Typeable)
import Data.String.Conversions (LBS)

import Thentos.Types


-- FIXME: workaround implementation until #193 is fixed:

data ThentosError' =
      A3BackendErrorResponse Int LBS
    | A3BackendInvalidJson String
  deriving (Eq, Show, Typeable)

throwA3Error :: ThentosError' -> a
throwA3Error = error . show

throwCoreError :: (e ~ ThentosError DB, MonadError e m) => e -> m a
throwCoreError = throwError



{-

-- the following should work once #193 is fixed.

module Thentos.Adhocracy3.Types
    ( module Core
    , DB(..)
    , ThentosError(..)
    , throwA3Error
    , throwCoreError
    )
    where

import Control.Exception (Exception)
import Control.Lens (makeLenses)
import Control.Monad.Except (MonadError, throwError)
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

throwA3Error :: (e ~ ThentosError DB, MonadError e m) => e -> m a
throwA3Error = throwError

throwCoreError :: (e ~ ThentosError Core.DB, e' ~ ThentosError DB, MonadError e' m) => e -> m a
throwCoreError = throwError . ThentosA3ErrorCore

makeLenses ''DB

$(deriveSafeCopy 0 'base ''DB)

-}
