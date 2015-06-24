{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}

module Thentos.Transaction.Core
  ( ThentosUpdate, ThentosUpdate'
  , ThentosQuery, ThentosQuery'
  , liftThentosQuery
  , runThentosUpdate
  , runThentosQuery
  , polyUpdate
  , polyQuery
  ) where

import Control.Applicative ((<$>))
import Control.Lens ((^.), (%%~))
import Control.Monad.Identity (Identity(Identity), runIdentity)
import Control.Monad.Reader (ReaderT(ReaderT), runReaderT, ask)
import Control.Monad.State (StateT(StateT), runStateT, get, put)
import Control.Monad.Trans.Either (EitherT(EitherT), runEitherT)
import Data.Acid (Update, Query)

import Thentos.Types


-- * types

type ThentosUpdate db a = ThentosUpdate' db ThentosError a
type ThentosQuery  db a = ThentosQuery'  db ThentosError a

type ThentosUpdate' db e a = EitherT e (StateT  db Identity) a
type ThentosQuery'  db e a = EitherT e (ReaderT db Identity) a

-- FUTURE WORK: make primed types newtypes rather than type synonyms, and provide a generic monad
-- instance.  (how does that work?)


-- * plumbing

-- | 'liftQuery' for 'ThentosUpdate' and 'ThentosUpdate''.
liftThentosQuery :: ThentosQuery' db e a -> ThentosUpdate' db e a
liftThentosQuery thentosQuery = EitherT . StateT $ \ state ->
    (, state) <$> runEitherT thentosQuery `runReaderT` state


-- | Push 'ThentosUpdate' event down to acid-state's own 'Update'.  Errors are returned as 'Left'
-- values in an 'Either'.  See also:
--
-- - http://www.reddit.com/r/haskell/comments/2re0da/error_handling_in_acidstate/
-- - http://petterbergman.se/aciderror.html.en
-- - http://acid-state.seize.it/Error%20Scenarios
-- - https://github.com/acid-state/acid-state/pull/38
runThentosUpdate :: ThentosUpdate db a -> Update db (Either ThentosError a)
runThentosUpdate action = do
    state <- get
    case runIdentity $ runStateT (runEitherT action) state of
        (Left err,     _)      ->                return $ Left  err
        (Right result, state') -> put state' >> (return $ Right result)

-- | 'runThentosUpdate' for 'ThentosQuery' and 'ThentosQuery''
runThentosQuery :: ThentosQuery db a -> Query db (Either ThentosError a)
runThentosQuery action = runIdentity . runReaderT (runEitherT action) <$> ask


-- | Turn an update transaction on 'DB' into one on any 'AsDB' instance.  See also 'polyQuery'.
--
-- FUTURE WORK:
--
--  1. shouldn't there be a way to do both cases with one function @poly@?
--  2. shouldn't there be a way to make acid-state events polymorphic in the state type?  (first try
--     without the template haskell magic, then, if that works, it should also work magically.)
polyUpdate :: forall e a db . AsDB db => ThentosUpdate' DB e a -> ThentosUpdate' db e a
polyUpdate upd = EitherT . StateT $ Identity . (asDB %%~ bare)
  where
    bare :: DB -> (Either e a, DB)
    bare = runIdentity . runStateT (runEitherT upd)

-- | Turn a query transaction on 'DB' into one on any 'AsDB' instance.  See also 'polyUpdate'.
polyQuery :: AsDB db => ThentosQuery' DB e a -> ThentosQuery' db e a
polyQuery qry = EitherT . ReaderT $ \ (state :: db) ->
    runEitherT qry `runReaderT` (state ^. asDB)
