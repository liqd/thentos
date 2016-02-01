{- Safe -}

{-# LANGUAGE ConstraintKinds             #-}
{-# LANGUAGE DeriveFunctor               #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE PackageImports              #-}
{-# LANGUAGE TemplateHaskell             #-}

module Thentos.Action.Types where

import Control.Concurrent (MVar)
import Control.Exception (Exception, SomeException)
import Control.Lens (makeLenses)
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


data ActionState =
    ActionState
      { _aStConfig  :: ThentosConfig
      , _aStRandom  :: MVar ChaChaDRG
      , _aStDb      :: Pool Connection
      }
  deriving (Generic)

makeLenses ''ActionState


-- | The 'Action' monad transformer stack.  It contains:
--
--     - 'LIO' as a base monad.
--     - A state of polymorphic type (for use e.g. by the frontend handlers to store cookies etc.)
--     - The option of throwing @ThentosError e@.  (Not 'ActionError e', which contains
--       authorization errors that must not be catchable from inside an 'Action'.)
--     - An 'ActionState' in a reader.  The state can be used by actions for calls to 'LIO', which
--       will have authorized effect.  Since it is contained in a reader, actions do not have the
--       power to corrupt it.
newtype Action e s a =
    Action
      { fromAction :: ReaderT ActionState
                          (EitherT (ThentosError e)
                              (StateT s
                                  (LIO DCLabel))) a
      }
  deriving (Functor, Generic)

instance Applicative (Action e s) where
    pure = Action . pure
    (Action ua) <*> (Action ua') = Action $ ua <*> ua'

instance Monad (Action e s) where
    return = pure
    (Action ua) >>= f = Action $ ua >>= fromAction . f

instance MonadReader ActionState (Action e s) where
    ask = Action ask
    local f = Action . local f . fromAction

instance MonadError (ThentosError e) (Action e s) where
    throwError = Action . throwError
    catchError (Action ua) h = Action $ catchError ua (fromAction . h)

instance MonadState s (Action e s) where
    state = Action . state

instance MonadLIO DCLabel (Action e s) where
    liftLIO lio = Action . ReaderT $ \_ -> EitherT (Right <$> lift lio)

-- FIXME: use me instead of 'Action'.
type MonadAction e s m =
    (MonadReader ActionState m,
     MonadError (ThentosError e) m,
     MonadState s m,
     MonadLIO DCLabel m)

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
  deriving (Show)

instance (Show e, Typeable e) => Exception (ActionError e)


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
