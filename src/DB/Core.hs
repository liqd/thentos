{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module DB.Core
  ( DbError(..)
  , ThentosClearance(..)
  , ThentosUpdate
  , runThentosUpdate
  , ThentosQuery
  , runThentosQuery
  , liftThentosQuery
  , showDbError
  , returnDBQ
  , throwDBQ
  , returnDBU
  , throwDBU
  , thentosPublic
  , thentosDenied
  , thentosLabeledPublic
  , thentosLabeledDenied
  , createCheckpointLoop
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay, forkIO, ThreadId)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State (StateT(StateT), runStateT, get, put, lift)
import Control.Monad.Trans.Either (EitherT, left, right, runEitherT)
import Data.Acid (AcidState, Update, Query, createCheckpoint)
import Data.SafeCopy (SafeCopy, contain, putCopy, getCopy, safePut, safeGet)
import Data.Typeable (Typeable)
import LIO (canFlowTo)
import LIO.DCLabel (DCLabel, (%%))
import Safe (readMay)

import Debug.Trace (traceShow)  -- to dump authorization errors to
                                -- stdout for debugging until we have
                                -- proper logging.

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
    | PermissionDenied ThentosClearance ThentosLabel
    | BadCredentials
    | BadAuthenticationHeaders
    | UidOverflow
    deriving (Eq, Ord, Show, Read, Typeable)

instance SafeCopy DbError
  where
    putCopy = contain . safePut . show
    getCopy = contain $ safeGet >>= \ raw ->
      maybe (fail $ "instance SafeCopy DbError: no parse" ++ show raw) return . readMay $ raw


type ThentosUpdate a = StateT  DB (EitherT (ThentosLabeled DbError) Identity) (ThentosLabeled a)
type ThentosQuery  a = ReaderT DB (EitherT (ThentosLabeled DbError) Identity) (ThentosLabeled a)


-- * plumbing

liftThentosQuery :: forall a . ThentosQuery a -> ThentosUpdate a
liftThentosQuery thentosQuery = StateT $ \ state ->
    (, state) <$> thentosQuery `runReaderT` state


-- | the type of this will change when servant has a better error type.
showDbError :: DbError -> (Int, String)
showDbError NoSuchUser               = (404, "user not found")
showDbError NoSuchService            = (404, "service not found")
showDbError NoSuchSession            = (404, "session not found")
showDbError UserAlreadyExists        = (403, "user already exists")
showDbError ServiceAlreadyExists     = (403, "service already exists")
showDbError SessionAlreadyExists     = (403, "session already exists")
showDbError UserEmailAlreadyExists   = (403, "email already in use")
showDbError e@(PermissionDenied _ _) = traceShow e (401, "unauthorized")
showDbError BadCredentials           = (401, "unauthorized")
showDbError BadAuthenticationHeaders = (400, "bad authentication headers")
showDbError UidOverflow              = (500, "internal error: UidOverflow")

    -- FIXME: get rid of 'SessionAlreadyExists'.  startSession should
    -- return the active token and update the timeout like with any
    -- other activity.

    -- FIXME: get rid of 'UidOverflow'.


-- | FIXME: generalize, so we can use this for both Update and Query.
-- (remove 'runThentosQuery' and 'ThentosQuery' when done.)
runThentosUpdate :: forall a . ThentosClearance -> ThentosUpdate a -> Update DB (Either DbError a)
runThentosUpdate clearance action = do
    state <- get
    case runIdentity . runEitherT $ runStateT action state of
        Left (ThentosLabeled label (err :: DbError)) ->
            checkClearance clearance label (return $ Left err)
        Right (ThentosLabeled label result, state') ->
            checkClearance clearance label (put state' >> return (Right result))

runThentosQuery :: forall a . ThentosClearance -> ThentosQuery a -> Query DB (Either DbError a)
runThentosQuery clearance action = do
    state <- ask
    case runIdentity . runEitherT $ runReaderT action state of
        Left (ThentosLabeled label (err :: DbError)) ->
            checkClearance clearance label (return $ Left err)
        Right (ThentosLabeled label result) ->
            checkClearance clearance label (return $ Right result)

checkClearance :: Monad m => ThentosClearance -> ThentosLabel -> m (Either DbError a) -> m (Either DbError a)
checkClearance clearance label result =
    if (fromThentosLabel label) `canFlowTo` (fromThentosClearance clearance)
        then result
        else return . Left $ PermissionDenied clearance label

throwDBU :: DCLabel -> DbError -> ThentosUpdate a
throwDBU label = lift . left . thentosLabeled label

returnDBU :: DCLabel -> a -> ThentosUpdate a
returnDBU label = lift . right . thentosLabeled label

throwDBQ :: DCLabel -> DbError -> ThentosQuery a
throwDBQ label = lift . left . thentosLabeled label

returnDBQ :: DCLabel -> a -> ThentosQuery a
returnDBQ label = lift . right . thentosLabeled label


thentosPublic :: DCLabel
thentosPublic = True %% False

thentosDenied :: DCLabel
thentosDenied = False %% True

thentosLabeledPublic :: t -> ThentosLabeled t
thentosLabeledPublic = thentosLabeled thentosPublic

thentosLabeledDenied :: t -> ThentosLabeled t
thentosLabeledDenied = thentosLabeled thentosDenied


-- * convenience

-- | Create a new thread that calls `createCheckpoint` synchronously,
-- then waits for @timeThreshold@ miliseconds, then repeats.  If
-- @sizeThreshold@ is `Just` a size, create checkpoint only if size of
-- segment of current change log since last checkpoint is larger than
-- that.
--
-- FIXME: check change log size.  (i think this is only possible
-- inside acid-state.)  https://github.com/acid-state/acid-state.
createCheckpointLoop :: AcidState st -> Int -> Maybe Int -> IO ThreadId
createCheckpointLoop acidState timeThreshold _ = forkIO iter
  where
    iter = do
      threadDelay $ timeThreshold * 1000

      -- when (isJust sizeThreshold) . assert False $
      --   print "createCheckpointLoop: sizeThreshold handling not implemented."

      createCheckpoint acidState
      iter
