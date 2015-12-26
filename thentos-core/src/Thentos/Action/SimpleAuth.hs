{- LANGUAGE Safe                        #-}

{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE DeriveFunctor               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}

-- | Simplified access to 'Action' with guarded exits.
module Thentos.Action.SimpleAuth
  ( UnsafeAction(..)
  , assertAuth
  , hasAgent
  , hasUserId
  , hasServiceId
  , hasRole
  , hasPrivilegedIP
  ) where

import Control.Conditional (ifM)
import LIO.Core (liftLIO, taint)
import LIO.DCLabel ((%%))

import LIO.Missing
import Thentos.Action.Types
import Thentos.Backend.Api.Auth.Types
import Thentos.Types


-- | Run boolean authorization predicate.  Throw 'ActionErrorAnyLabel' if the result is 'False'.
assertAuth :: Action e s Bool -> Action e s ()
assertAuth utest = ifM utest (pure ()) (liftLIO $ taint dcTop)

hasAgent :: Agent -> Action e s Bool
hasAgent (UserA u) = hasUserId u
hasAgent (ServiceA s) = hasServiceId s

hasUserId :: UserId -> Action e s Bool
hasUserId uid = guardWriteOk (UserA uid %% UserA uid)

hasServiceId :: ServiceId -> Action e s Bool
hasServiceId sid = guardWriteOk (ServiceA sid %% ServiceA sid)

hasRole :: Role -> Action e s Bool
hasRole role = guardWriteOk (role %% role)

hasPrivilegedIP :: Action e s Bool
hasPrivilegedIP = guardWriteOk (PrivilegedIP %% PrivilegedIP)
