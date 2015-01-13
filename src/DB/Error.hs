{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module DB.Error
where

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
    | SessionAlreadyExists
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
        Left (ThentosLabeled (ThentosLabel label) (err :: DbError)) ->
            checkClearance label (return $ Left err)
        Right (ThentosLabeled (ThentosLabel label) result, state') ->
            checkClearance label $ put state' >> (return $ Right result)
  where
    checkClearance :: DCLabel -> Update DB (Either DbError a) -> Update DB (Either DbError a)
    checkClearance label result = if label `canFlowTo` clearance
        then result
        else return $ Left PermissionDenied

runThentosQuery :: forall a . ThentosClearance -> ThentosQuery a -> Query DB (Either DbError a)
runThentosQuery (ThentosClearance (ThentosLabel clearance)) action = do
    state <- ask
    let result :: Either (ThentosLabeled DbError) (ThentosLabeled a)
        result = runIdentity . runEitherT $ runReaderT action state
    case result of
        Left (ThentosLabeled (ThentosLabel label) (err :: DbError)) ->
            checkClearance label (return $ Left err)
        Right (ThentosLabeled (ThentosLabel label) result) ->
            checkClearance label (return $ Right result)
  where
    checkClearance :: DCLabel -> Query DB (Either DbError a) -> Query DB (Either DbError a)
    checkClearance label result = if label `canFlowTo` clearance
        then result
        else return $ Left PermissionDenied

throwDB :: DCLabel -> DbError -> ThentosUpdate a
throwDB label = lift . left . thentosLabeled label

returnDB :: DCLabel -> a -> ThentosUpdate a
returnDB label = lift . right . thentosLabeled label

throwDBQ :: DCLabel -> DbError -> ThentosQuery a
throwDBQ label = lift . left . thentosLabeled label

returnDBQ :: DCLabel -> a -> ThentosQuery a
returnDBQ label = lift . right . thentosLabeled label

-- | we should not have to write this ourselves...
when' :: (Functor m, Monad m) => Bool -> m a -> m ()
when' True action = void action
when' False _ = return ()
