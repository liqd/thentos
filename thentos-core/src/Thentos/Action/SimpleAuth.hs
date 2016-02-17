{- Safe -}

{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}

-- | Simplified access to 'Action' with guarded exits.
module Thentos.Action.SimpleAuth
  ( assertAuth
  , hasAgent
  , hasUserId
  , hasServiceId
  , hasGroup
  , hasPrivilegedIP
  ) where

import Control.Conditional (ifM)
import LIO.Core (liftLIO, taint)
import LIO.DCLabel ((%%))

import LIO.Missing
import Thentos.Backend.Api.Auth.Types
import Thentos.Types
import Thentos.Action.Types (MonadThentosIO)


-- | Run boolean authorization predicate.  Throw 'ActionErrorAnyLabel' if the result is 'False'.
assertAuth :: MonadThentosIO m => m Bool -> m ()
assertAuth utest = ifM utest (pure ()) (liftLIO $ taint dcTop)

hasAgent :: MonadThentosIO m => Agent -> m Bool
hasAgent (UserA u) = hasUserId u
hasAgent (ServiceA s) = hasServiceId s

hasUserId :: MonadThentosIO m => UserId -> m Bool
hasUserId uid = guardWriteOk (UserA uid %% UserA uid)

hasServiceId :: MonadThentosIO m => ServiceId -> m Bool
hasServiceId sid = guardWriteOk (ServiceA sid %% ServiceA sid)

hasGroup :: MonadThentosIO m => Group -> m Bool
hasGroup g = guardWriteOk (g %% g)

hasPrivilegedIP :: MonadThentosIO m => m Bool
hasPrivilegedIP = guardWriteOk (PrivilegedIP %% PrivilegedIP)
