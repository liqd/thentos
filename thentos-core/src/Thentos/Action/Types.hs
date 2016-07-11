{- Safe -}

{-# LANGUAGE ConstraintKinds             #-}
{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveFunctor               #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE TemplateHaskell             #-}

module Thentos.Action.Types where

import Control.Exception (SomeException)
import Control.Monad.Reader (ReaderT(ReaderT))
import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT(EitherT))
import Data.Configifier ((>>.))
import Database.PostgreSQL.Simple (Connection)
import Data.Pool (Pool)
import LIO.Core (LIO)
import LIO.TCB (ioTCB)

import Thentos.Types
import Thentos.Config
import Thentos.Prelude
import Thentos.Frontend.Session.CSRF


data ActionEnv =
    ActionEnv
      { _aStConfig  :: ThentosConfig
      , _aStDb      :: Pool Connection
      }
  deriving (Generic)

makeLenses ''ActionEnv

class GetThentosDb a where
    getThentosDb :: Getter a (Pool Connection)

instance GetThentosDb ActionEnv where
    getThentosDb = aStDb

class GetThentosConfig a where
    getThentosConfig :: Getter a ThentosConfig

instance GetThentosConfig ActionEnv where
    getThentosConfig = aStConfig

type MonadThentosConfig v m = (MonadReader v m, GetThentosConfig v)

instance GetCsrfSecret ActionEnv where
    csrfSecret = pre $ aStConfig . to (>>. (Proxy :: Proxy '["csrf_secret"])) . _Just . csrfSecret . _Just


-- | The 'Action' monad transformer stack.  It contains:
--
--     - 'LIO' as a base monad.
--     - A state of polymorphic type (for use e.g. by the frontend handlers to store cookies etc.)
--     - The option of throwing @ThentosError e@.  (Not 'ActionError e', which contains
--       authorization errors that must not be catchable from inside an 'Action'.)
--     - An 'ActionEnv' in a reader.  The state can be used by actions for calls to 'LIO', which
--       will have authorized effect.  Since it is contained in a reader, actions do not have the
--       power to corrupt it.
newtype ActionStack e s a =
    ActionStack
      { fromAction :: ReaderT ActionEnv
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

instance MonadReader ActionEnv (ActionStack e s) where
    ask = ActionStack ask
    local f = ActionStack . local f . fromAction

instance MonadError (ThentosError e) (ActionStack e s) where
    throwError = ActionStack . throwError
    catchError (ActionStack ua) h = ActionStack $ catchError ua (fromAction . h)

instance MonadState s (ActionStack e s) where
    state = ActionStack . state

instance MonadLIO DCLabel (ActionStack e s) where
    liftLIO lio = ActionStack . ReaderT $ \_ -> EitherT (Right <$> lift lio)

instance MonadRandom (ActionStack e s) where
    getRandomBytes = liftLIO . ioTCB . getRandomBytes

type MonadQuery e v m =
    (GetThentosDb v,
     GetThentosConfig v,
     MonadReader v m,
     MonadThentosError e m,
     MonadThentosIO m)

type MonadAction e v m = (MonadQuery e v m, MonadRandom m)

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
