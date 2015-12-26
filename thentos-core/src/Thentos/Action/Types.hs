{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE PackageImports              #-}

module Thentos.Action.Types where

import Control.Concurrent (MVar)
import Control.Exception (Exception, SomeException)
import Control.Monad.Except (MonadError, throwError, catchError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT(ReaderT), MonadReader, ask, local)
import Control.Monad.State (MonadState, StateT, state)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT(EitherT))
import "cryptonite" Crypto.Random (ChaChaDRG)
import Database.PostgreSQL.Simple (Connection)
import Data.Pool (Pool)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import LIO.Core (MonadLIO, LIO, liftLIO)
import LIO.DCLabel (DCLabel)
import LIO.Error (AnyLabelError)

import Thentos.Types
import Thentos.Config


-- FIXME: we should make this a record (instead of a newtype around a tuple)
-- so we can get the fields without having to pattern-match
newtype ActionState =
    ActionState
      { fromActionState :: (Pool Connection, MVar ChaChaDRG, ThentosConfig)
      }
  deriving (Typeable, Generic)

-- | The 'Action' monad transformer stack.  It contains:
--
--     - 'LIO' as a base monad.
--     - A state of polymorphic type (for use e.g. by the frontend handlers to store cookies etc.)
--     - The option of throwing @ThentosError e@.  (Not 'ActionError e', which contains
--       authorization errors that must not be catchable from inside an 'Action'.)
--     - An 'ActionState' in a reader.  The state can be used by actions for calls to 'LIO', which
--       will have authorized effect.  Since it is contained in a reader, actions do not have the
--       power to corrupt it.
--
-- FIXME: make this a data type with explicit instance and tag this module @Safe@.
newtype Action e s a =
    Action
      { fromAction :: ReaderT ActionState
                          (EitherT (ThentosError e)
                              (StateT s
                                  (LIO DCLabel))) a
      }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader ActionState
           , MonadError (ThentosError e)
           , MonadState s
           , Typeable
           , Generic
           )

-- | Errors known by 'runActionE', 'runAction', ....
--
-- The 'MonadError' instance of newtype 'Action' lets you throw and catch errors from *within* the
-- 'Action', i.e., at construction time).  These are errors are handled in the 'ActionErrorThentos'
-- constructor.  Label errors and other (possibly async) exceptions are caught (if possible) in
-- 'runActionE' and its friends and maintained in other 'ActionError' constructors.
data ActionError e =
    ActionErrorThentos (ThentosError e)
  | ActionErrorAnyLabel AnyLabelError
  | ActionErrorUnknown SomeException
  deriving ( Show
           , Typeable
           )

instance (Show e, Typeable e) => Exception (ActionError e)

instance MonadLIO DCLabel (Action e s) where
    liftLIO lio = Action . ReaderT $ \_ -> EitherT (Right <$> lift lio)


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
