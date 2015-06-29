{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}

{-# OPTIONS -fno-warn-orphans #-}

-- (to enable orphan-instance warning, we could try to make module
-- Core parametric in DB.  but that has its own obvious
-- disadvantages.)

-- | This module provides a basic collection of acid-state transactions on the state type 'DB'.
--
-- For each transaction, we export the acid-state constructor @FooBloo@ that can be be passed to
-- 'query' or 'update' and a function 'trans_fooBloo' that can be used as a building block for
-- writing new transactions.
--
-- Lookup functions usually return the lookup key together with the value.  The key can be discarded
-- easy enough using pattern matching, but in some cases it is useful to have it.

module Thentos.Transaction
    ( module Thentos.Transaction
    , module Thentos.Transaction.Transactions
    )
where

import Data.Data (Typeable)
import Thentos.Transaction.Core (runThentosUpdate, runThentosQuery)
import Thentos.Transaction.Transactions
import Thentos.Transaction.TH
import Thentos.Types

import qualified Data.Set as Set

import Data.SafeCopy (SafeCopy, deriveSafeCopy, base)

import Data.Acid
import Data.Acid.Advanced

$(makeThentosAcidicPhase1 transaction_names)
$(makeThentosAcidicPhase2 ''DB transaction_names)

{-
data AgentRoles t = AgentRoles Agent
    deriving Typeable

data LookupThentosSession t = LookupThentosSession Timestamp ThentosSessionToken
    deriving Typeable

$(deriveSafeCopy 0 'base ''AgentRoles)
$(deriveSafeCopy 0 'base ''LookupThentosSession)

instance (db `Extends` DB) => Method (AgentRoles db) where
    type MethodResult (AgentRoles db) = Either (ThentosError db) (Set.Set Role)
    type MethodState (AgentRoles db) = db

instance (db `Extends` DB) => Method (LookupThentosSession db) where
    type MethodResult (LookupThentosSession db) = Either (ThentosError db) (ThentosSessionToken, ThentosSession)
    type MethodState (LookupThentosSession db) = db

instance (db `Extends` DB) => QueryEvent (AgentRoles db)
instance (db `Extends` DB) => UpdateEvent (LookupThentosSession db)

dbEvents :: (db `Extends` DB) => [Event db]
dbEvents =
    [ QueryEvent (\(AgentRoles a) -> agentRoles a)
    , UpdateEvent (\(LookupThentosSession now tok) -> lookupThentosSession now tok)
    ]

instance IsAcidic DB where
    acidEvents = dbEvents
-- $(makeThentosAcidicPhase2 transaction_names)
-}

{-
-- So for every transaction, we need the following:

data AgentRoles t = AgentRoles Agent
    deriving Typeable

$(deriveSafeCopy 0 'base ''AgentRoles)

instance (db `Extends` DB) => Method (AgentRoles db) where
    type MethodResult (AgentRoles db) = Either (ThentosError db) (Set.Set Role)
    type MethodState (AgentRoles db) = db

instance (db `Extends` DB) => QueryEvent (AgentRoles db)
    -- or UpdateEvent, as the case may be

-- and then a list of all events/transactions:
dbEvents :: (db `Extends` DB) => [Event db]
dbEvents =
    [ QueryEvent (\(AgentRoles a) -> agentRoles a)
    ..
    ]

-- the TH should probably take the name of the base db type as an argument and
-- substitute that in all the places that say "DB" above

-}
