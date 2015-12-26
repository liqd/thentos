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
import Control.Monad.Except (MonadError, throwError, catchError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, MonadReader, ask, local)
import Control.Monad.State (MonadState, StateT, state)
import Control.Monad.Trans.Either (EitherT)
import GHC.Generics (Generic)
import LIO.Core (liftLIO, taint)
import LIO.DCLabel ((%%))

import LIO.Missing
import Thentos.Action.Core
import Thentos.Backend.Api.Auth.Types
import Thentos.Types


-- * type

-- | Like 'Action', but with 'IO' at the base.
newtype UnsafeAction e s a =
    UnsafeAction
      { fromUnsafeAction :: ReaderT ActionState
                                (EitherT (ThentosError e)
                                    (StateT s
                                        IO)) a
      }
  deriving (Functor, Generic)

instance Applicative (UnsafeAction e s) where
    pure = UnsafeAction . pure
    (UnsafeAction ua) <*> (UnsafeAction ua') = UnsafeAction $ ua <*> ua'

instance Monad (UnsafeAction e s) where
    return = pure
    (UnsafeAction ua) >>= f = UnsafeAction $ ua >>= fromUnsafeAction . f

instance MonadReader ActionState (UnsafeAction e s) where
    ask = UnsafeAction ask
    local f = UnsafeAction . local f . fromUnsafeAction

instance MonadError (ThentosError e) (UnsafeAction e s) where
    throwError = UnsafeAction . throwError
    catchError (UnsafeAction ua) h = UnsafeAction $ catchError ua (fromUnsafeAction . h)

instance MonadState s (UnsafeAction e s) where
    state = UnsafeAction . state

instance MonadIO (UnsafeAction e s) where
    liftIO = UnsafeAction . liftIO


-- * authorization predicates

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
