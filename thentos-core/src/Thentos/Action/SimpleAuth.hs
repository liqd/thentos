{-# LANGUAGE Unsafe                      #-}

{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveFunctor               #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TypeFamilies                #-}

{-| Simplified access to 'Action' with guarded exits.

-}
module Thentos.Action.SimpleAuth
  ( UnsafeAction(..)
  , assertAuth
  , hasAgent
  , hasUserId
  , hasServiceId
  , hasRole
  , guardedUnsafeAction
  , unsafeAction
  ) where

import Control.Conditional (ifM)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT(ReaderT), MonadReader, runReaderT)
import Control.Monad.State (MonadState, StateT(StateT), runStateT)
import Control.Monad.Trans.Either (EitherT(EitherT), runEitherT)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import LIO.Core (liftLIO, taint)
import LIO.DCLabel ((%%))
import LIO.TCB (ioTCB)

import LIO.Missing
import Thentos.Action.Core
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
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader ActionState
           , MonadError (ThentosError e)
           , MonadState s
           , MonadIO
           , Typeable
           , Generic
           )


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


-- * making unsafe actions safe

-- | Run an 'UnsafeAction' in a safe 'Action' with extra authorization checks (performed through
-- 'assertAuth').
guardedUnsafeAction :: Action e s Bool -> UnsafeAction e s a -> Action e s a
guardedUnsafeAction utest uaction = assertAuth utest >> unsafeAction uaction

-- | Run an 'UnsafeAction' in a safe 'Action' without extra authorization checks.
unsafeAction :: UnsafeAction e a -> Action e a
unsafeAction uaction = construct deconstruct
  where
    construct io = Action . ReaderT $ EitherT . ioTCB . io
    deconstruct = runEitherT . runReaderT (fromUnsafeAction uaction)
