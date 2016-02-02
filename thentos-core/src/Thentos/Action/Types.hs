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
newtype ActionStack e s a =
    ActionStack
      { fromAction :: ReaderT ActionState
                          (EitherT (ThentosError e)
                              (StateT s
                                  (LIO DCLabel))) a
      }
  deriving (Functor, Generic)

instance Applicative (ActionStack e s) where
    pure = ActionStack . pure
    (ActionStack ua) <*> (ActionStack ua') = ActionStack $ ua <*> ua'

instance Monad (ActionStack e s) where
    return = pure
    (ActionStack ua) >>= f = ActionStack $ ua >>= fromAction . f

instance MonadReader ActionState (ActionStack e s) where
    ask = ActionStack ask
    local f = ActionStack . local f . fromAction

instance MonadError (ThentosError e) (ActionStack e s) where
    throwError = ActionStack . throwError
    catchError (ActionStack ua) h = ActionStack $ catchError ua (fromAction . h)

instance MonadState s (ActionStack e s) where
    state = ActionStack . state

instance MonadLIO DCLabel (ActionStack e s) where
    liftLIO lio = ActionStack . ReaderT $ \_ -> EitherT (Right <$> lift lio)

type MonadThentosIO m = MonadLIO DCLabel m
type MonadThentosReader m = MonadReader ActionState m

type MonadQuery e m =
    (MonadThentosReader m,
     MonadThentosError e m,
     MonadThentosIO m)

type MonadAction e s m =
    (MonadThentosReader m,
     MonadThentosError e m,
     MonadState s m,
     MonadThentosIO m)

-- | Errors known by 'runActionE', 'runAction', ....
--
-- FIXME DOC
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
