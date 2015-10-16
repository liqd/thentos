{-# LANGUAGE Unsafe                      #-}

{-# LANGUAGE ConstraintKinds             #-}
{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveFunctor               #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE InstanceSigs                #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE UndecidableInstances        #-}

{-| Simplified access to 'Action' with guarded exits.

-}
module Thentos.Action.SimpleCheckClearance
  ( UnsafeAction(..)

  , isUserLoggedIn
  , doesUserHaveId
  , doesUserHaveRole

  , assertAuth
  , guardedUnsafeAction
  , unsafeAction
  ) where

import Control.Conditional (ifM)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT(ReaderT), MonadReader, runReaderT)
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
newtype UnsafeAction e a =
    UnsafeAction
      { fromUnsafeAction :: ReaderT ActionState (EitherT (ThentosError e) IO) a
      }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader ActionState
           , MonadError (ThentosError e)
           , MonadIO
           , Typeable
           , Generic
           )


-- * authorization predicates

-- | Run boolean authorization predicate.  Throw 'ActionErrorAnyLabel' if the result is 'False'.
assertAuth :: (e ~ ActionError e') => Action e Bool -> Action e ()
assertAuth utest = ifM utest (pure ()) (liftLIO $ taint dcTop)

isUserLoggedIn :: Action e Bool
isUserLoggedIn = doesUserHaveRole RoleUser

doesUserHaveId :: UserId -> Action e Bool
doesUserHaveId uid = guardWriteOk (UserA uid %% UserA uid)

doesUserHaveRole :: Role -> Action e Bool
doesUserHaveRole role = guardWriteOk (role %% role)


-- * making unsafe actions safe

-- | Run an 'UnsafeAction' in a safe 'Action' with extra authorization checks (performed through
-- 'assertAuth').
guardedUnsafeAction :: (e ~ ActionError e') => Action e Bool -> UnsafeAction e a -> Action e a
guardedUnsafeAction utest uaction = assertAuth utest >> unsafeAction uaction

-- | Run an 'UnsafeAction' in a safe 'Action' without extra authorization checks.
unsafeAction :: (e ~ ActionError e') => UnsafeAction e a -> Action e a
unsafeAction uaction = construct deconstruct
  where
    construct io = Action . ReaderT $ EitherT . ioTCB . io
    deconstruct = runEitherT . runReaderT (fromUnsafeAction uaction)
