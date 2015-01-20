{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module DB.Error (
    DbError(..),
    ThentosClearance(..),
    ThentosUpdate,
    runThentosUpdate,
    ThentosQuery,
    runThentosQuery,
    liftThentosQuery,
    returnDBQ,
    throwDBQ,
    returnDBU,
    throwDBU,
    when'
) where

import Control.Applicative ((<$>))
import Control.Monad.Identity (Identity, runIdentity, void)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State (StateT(StateT), runStateT, get, put, lift)
import Control.Monad.Trans.Either (EitherT, left, right, runEitherT)
import Data.Acid (Update, Query)
import Data.SafeCopy (SafeCopy, contain, putCopy, getCopy, safePut, safeGet)
import Data.Typeable (Typeable)
import LIO (canFlowTo)
import LIO.DCLabel (DCLabel)
import Safe (readMay)

import Types


-- * types

data DbError =
      NoSuchUser
    | NoSuchService
    | NoSuchSession
    | UserAlreadyExists
    | ServiceAlreadyExists
    | SessionAlreadyExists
    | UserEmailAlreadyExists
    | PermissionDenied
    | UidOverflow
    deriving (Eq, Ord, Enum, Show, Read, Typeable)

instance SafeCopy DbError
  where
    putCopy = contain . safePut . show
    getCopy = contain $ safeGet >>= \ raw ->
      maybe (fail $ "instance SafeCopy DbError: no parse" ++ show raw) return . readMay $ raw

newtype ThentosClearance = ThentosClearance ThentosLabel
    deriving (Eq, Ord, Show, Read)

instance SafeCopy ThentosClearance
  where
    putCopy = contain . safePut . show
    getCopy = contain $ safeGet >>= \ raw ->
      maybe (fail $ "instance SafeCopy DbError: no parse" ++ show raw) return . readMay $ raw


type ThentosUpdate a = StateT  DB (EitherT (ThentosLabeled DbError) Identity) (ThentosLabeled a)
type ThentosQuery  a = ReaderT DB (EitherT (ThentosLabeled DbError) Identity) (ThentosLabeled a)

liftThentosQuery :: forall a . ThentosQuery a -> ThentosUpdate a
liftThentosQuery thentosQuery = StateT $ \ state ->
    (, state) <$> thentosQuery `runReaderT` state


-- * plumbing

-- | FIXME: generalize, so we can use this for both Update and Query.
-- (remove 'runThentosQuery' and 'ThentosQuery' when done.)
runThentosUpdate :: forall a . ThentosClearance -> ThentosUpdate a -> Update DB (Either DbError a)
runThentosUpdate (ThentosClearance (ThentosLabel clearance)) action = do
    state <- get
    let result :: Either (ThentosLabeled DbError) (ThentosLabeled a, DB)
        result = runIdentity . runEitherT $ runStateT action state
    case result of
        Left (ThentosLabeled label (err :: DbError)) ->
            checkClearance ((`canFlowTo` clearance) . fromThentosLabel) label (return $ Left err)
        Right (ThentosLabeled label result, state') ->
            checkClearance  ((`canFlowTo` clearance) . fromThentosLabel) label $ put state' >> (return $ Right result)

runThentosQuery :: forall a . ThentosClearance -> ThentosQuery a -> Query DB (Either DbError a)
runThentosQuery (ThentosClearance (ThentosLabel clearance)) action = do
    state <- ask
    let result :: Either (ThentosLabeled DbError) (ThentosLabeled a)
        result = runIdentity . runEitherT $ runReaderT action state
    case result of
        Left (ThentosLabeled label (err :: DbError)) ->
            checkClearance ((`canFlowTo` clearance) . fromThentosLabel) label (return $ Left err)
        Right (ThentosLabeled label result) ->
            checkClearance ((`canFlowTo` clearance) . fromThentosLabel) label (return $ Right result)

checkClearance :: Monad m => (ThentosLabel -> Bool) -> ThentosLabel -> m (Either DbError a) -> m (Either DbError a)
checkClearance cleared label result = if cleared label
    then result
    else return $ Left PermissionDenied

throwDBU :: DCLabel -> DbError -> ThentosUpdate a
throwDBU label = lift . left . thentosLabeled label

returnDBU :: DCLabel -> a -> ThentosUpdate a
returnDBU label = lift . right . thentosLabeled label

throwDBQ :: DCLabel -> DbError -> ThentosQuery a
throwDBQ label = lift . left . thentosLabeled label

returnDBQ :: DCLabel -> a -> ThentosQuery a
returnDBQ label = lift . right . thentosLabeled label

-- | we should not have to write this ourselves...
when' :: (Functor m, Monad m) => Bool -> m a -> m ()
when' True action = void action
when' False _ = return ()
