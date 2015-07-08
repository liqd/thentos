{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

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

import Thentos.Transaction.Core (runThentosUpdate, runThentosQuery)
import Thentos.Transaction.Transactions
import Thentos.Transaction.TH
import Thentos.Types

$(makeThentosAcidicPhase1 ''DB transaction_names)
$(makeThentosAcidicPhase2 ''DB transaction_names [] [])
