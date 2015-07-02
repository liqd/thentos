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

module Thentos.Transaction.Types where

import qualified Thentos.Transaction.Transactions as T
import Thentos.Transaction.Core
import Thentos.Types
import Thentos.Transaction.TH
import Data.SafeCopy (SafeCopy(..), deriveSafeCopy, base)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Data.Functor.Infix ((<$>))

data CustomDB = CustomDB DB Int
  deriving (Eq, Show, Typeable, Generic)

instance CustomDB `Extends` DB where
    focus f (CustomDB db i) = (`CustomDB` i) <$> f db

    asDBThentosError :: ThentosError DB -> ThentosError CustomDB
    asDBThentosError = CustomDBError

instance CustomDB `Extends` CustomDB where
    focus = id
    asDBThentosError = id

data instance (ThentosError CustomDB) = CustomDBError { fromCustomDBError :: ThentosError DB }

deriving instance Eq (ThentosError CustomDB)
deriving instance Read (ThentosError CustomDB)
deriving instance Show (ThentosError CustomDB)

instance SafeCopy (ThentosError CustomDB)
  where
    putCopy = putCopyViaShowRead
    getCopy = getCopyViaShowRead

$(deriveSafeCopy 0 'base ''CustomDB)
$(makeThentosAcidicPhase1 T.transaction_names)
$(makeThentosAcidicPhase2 ''CustomDB T.transaction_names)
