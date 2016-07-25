{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Main where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State

throwError' :: (MonadState s m, MonadError (s, e) m) => e -> m a
throwError' e = do
    s <- get
    throwError (s, e)

-- s -> Either String (a, s)
newtype SE a = SE (StateT Int (ExceptT String Identity) a)
  deriving (Functor, Applicative, Monad, MonadState Int, MonadError String)

-- s -> (Either String a, s)
newtype ES a = ES (ExceptT String (StateT Int Identity) a)
  deriving (Functor, Applicative, Monad, MonadState Int, MonadError String)

sample :: (MonadState Int m, MonadError String m) => m Int
sample = do
  put 42
  catchError (modify (+1) >> throwError "Foo") (const $ modify (*2))
  get

runSE :: SE a -> Either String a
runSE (SE m) = runIdentity (runExceptT (flip evalStateT 0 m))

runES :: ES a -> Either String a
runES (ES m) = runIdentity (flip evalStateT 0 (runExceptT m))

-- What is the value of "runSE sample" and "runES sample", resp.?

-- runSE sample == Right 84
-- runES sample == Right 86

-- In the first line, the state is "reset", but not to 0, but to the value before 'catchError' was
-- entered.
