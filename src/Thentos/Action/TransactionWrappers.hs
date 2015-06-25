{-# LANGUAGE TemplateHaskell      #-}

module Thentos.Action.TransactionWrappers where

import Thentos.Action.Core (Action, query'P)
import Thentos.Types

import Thentos.Transaction (LookupUser(..))

import Thentos.Action.TH

-- TODO: get the (Constructor, Type) list from TH.Transaction.TH
-- and transfor the type as necessary (e.g. from 
--  UserId -> ThentosQuery db (UserId, User)
-- to
--  UserId -> Action db (UserId, User).

$(makeHasDBTypeClass [('LookupUser, lookupUserType)] [])
$(makeHasDBInstance ''DB [('LookupUser, lookupUserType)] [])

--class (AsDB db) => HasDB db where
--    lookupUser :: UserId -> Action db (UserId, User)

--instance HasDB DB where
--    lookupUser = query'P . T.LookupUser
