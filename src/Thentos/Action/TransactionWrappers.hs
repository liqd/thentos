module Thentos.Action.TransactionWrappers where

import Thentos.Action.Core (Action, query'P)
import Thentos.Types

import qualified Thentos.Transaction as T

class (AsDB db) => HasDB db where
    lookupUser :: UserId -> Action db (UserId, User)

instance HasDB DB where
    lookupUser = query'P . T.LookupUser
