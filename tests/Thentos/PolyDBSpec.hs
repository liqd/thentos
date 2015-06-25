{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}

{-# LANGUAGE TemplateHaskell      #-}

module Thentos.PolyDBSpec where

import Control.Applicative ((<$>))

import Thentos.Action (HasDB(..))
import Thentos.Action.Core
import Thentos.Types

import Data.Data (Typeable)
import GHC.Generics (Generic)
import Data.SafeCopy (deriveSafeCopy, base)

import Thentos.Transaction.TH
import Thentos.Transaction.Transactions (transaction_names)
import Thentos.Transaction.Core (runThentosQuery, runThentosUpdate)

import Test.Hspec (Spec)

import Thentos.Action.TH

spec :: Spec
spec = return ()

data MyDB = MyDB DB Int
  deriving (Eq, Show, Typeable, Generic)

$(deriveSafeCopy 0 'base ''MyDB)

$(makeThentosAcidicPhase1 ''MyDB transaction_names)
$(makeThentosAcidicPhase2 ''MyDB transaction_names)


instance AsDB MyDB where
    asDB f (MyDB db n) = (`MyDB` n) <$> f db

instance HasDB MyDB where
    lookupUser' = query'P . LookupUser

