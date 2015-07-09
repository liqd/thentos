{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | This defines a custom db type that extends the basic thentos DB. It is
-- used by the tests in the Thentos.Transaction.CoreSpec module.
module Thentos.Test.CustomDB where

import Control.Applicative ((<$>))
import Control.Exception (Exception)
import Control.Monad.Reader (ask)
import Data.SafeCopy (SafeCopy(..), deriveSafeCopy, base)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Thentos.Transaction.Core
import Thentos.Transaction.TH
import Thentos.Action.Core (ActionError(..))
import Thentos.Types

import qualified Thentos.Transaction as T

data CustomDB = CustomDB DB Int
  deriving (Eq, Show, Typeable, Generic)

instance CustomDB `Extends` DB where
    focus f (CustomDB db i) = (`CustomDB` i) <$> f db

    asDBThentosError :: ThentosError DB -> ThentosError CustomDB
    asDBThentosError = CustomDBError

instance EmptyDB CustomDB where
    emptyDB = CustomDB emptyDB 0

instance CustomDB `Extends` CustomDB where
    focus = id
    asDBThentosError = id

data instance (ThentosError CustomDB) = CustomDBError { fromCustomDBError :: ThentosError DB }

instance Exception (ThentosError CustomDB)

deriving instance Eq (ThentosError CustomDB)
deriving instance Read (ThentosError CustomDB)
deriving instance Show (ThentosError CustomDB)
deriving instance Show (ActionError CustomDB)

instance SafeCopy (ThentosError CustomDB)
  where
    putCopy = putCopyViaShowRead
    getCopy = getCopyViaShowRead


trans_getTheInt :: (db `Extends` CustomDB) => ThentosQuery db Int
trans_getTheInt = polyQuery $ do
    CustomDB _ n <- ask
    return n

$(deriveSafeCopy 0 'base ''CustomDB)
$(makeThentosAcidicPhase1 ''CustomDB ['trans_getTheInt])
$(makeThentosAcidicPhase2 ''CustomDB ['trans_getTheInt] [''DB] ['T.dbEvents])
