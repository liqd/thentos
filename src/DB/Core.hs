{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

module DB.Core
  ( DbError(..)
  , ThentosClearance(..)
  , ThentosUpdate, ThentosUpdate'
  , runThentosUpdate
  , ThentosQuery, ThentosQuery'
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
  , (=%%)
  , simpleThentosLabel
  , simpleLabel
  , createCheckpointLoop
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay, forkIO, ThreadId)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State (StateT(StateT), runStateT, get, put, lift)
import Control.Monad.Trans.Either (EitherT, left, right, runEitherT)
import Data.Acid (AcidState, Update, Query, createCheckpoint)
import Data.List (foldl')
import Data.SafeCopy (SafeCopy, contain, putCopy, getCopy, safePut, safeGet)
import Data.Typeable (Typeable)
import LIO (canFlowTo)
import LIO.DCLabel (DCLabel, ToCNF, (%%), (/\), (\/), toCNF)
import Safe (readMay)
import System.Log.Logger (Priority(INFO))
import System.Log.Missing (logger)

import Types


-- * types

data DbError =
      NoSuchUser
    | NoSuchService
    | NoSuchSession
    | UserAlreadyExists
    | ServiceAlreadyExists
    | UserEmailAlreadyExists
    | PermissionDenied ThentosClearance ThentosLabel
    | BadCredentials
    | BadAuthenticationHeaders
    | ProxyNotAvailable
    deriving (Eq, Ord, Show, Read, Typeable)

instance SafeCopy DbError
  where
    putCopy = contain . safePut . show
    getCopy = contain $ safeGet >>= \ raw ->
      maybe (fail $ "instance SafeCopy DbError: no parse" ++ show raw) return . readMay $ raw


type ThentosUpdate a = ThentosUpdate' (ThentosLabeled DbError) (ThentosLabeled a)
type ThentosQuery  a = ThentosQuery'  (ThentosLabeled DbError) (ThentosLabeled a)

type ThentosUpdate' e a = EitherT e (StateT  DB Identity) a
type ThentosQuery'  e a = EitherT e (ReaderT DB Identity) a


-- * plumbing

liftThentosQuery :: forall a . ThentosQuery a -> ThentosUpdate a
liftThentosQuery thentosQuery = StateT $ \ state ->
    (, state) <$> thentosQuery `runReaderT` state


-- | the type of this will change when servant has a better error type.
--
-- FIXME: this is backend specific.  or frontend specific.  i donno,
-- we probably need one for each, but not this one.
showDbError :: MonadIO m => DbError -> m (Int, String)
showDbError NoSuchUser               = return (404, "user not found")
showDbError NoSuchService            = return (404, "service not found")
showDbError NoSuchSession            = return (404, "session not found")
showDbError UserAlreadyExists        = return (403, "user already exists")
showDbError ServiceAlreadyExists     = return (403, "service already exists")
showDbError UserEmailAlreadyExists   = return (403, "email already in use")
showDbError e@(PermissionDenied _ _) = logger INFO (show e) >> return (401, "unauthorized")
showDbError BadCredentials           = return (401, "unauthorized")
showDbError BadAuthenticationHeaders = return (400, "bad authentication headers")
showDbError ProxyNotAvailable        = return (404, "proxying not activated")


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
    if fromThentosLabel label `canFlowTo` fromThentosClearance clearance
        then result
        else return . Left $ PermissionDenied clearance label

throwDBU :: ThentosLabel -> DbError -> ThentosUpdate a
throwDBU label = lift . left . ThentosLabeled label

returnDBU :: ThentosLabel -> a -> ThentosUpdate a
returnDBU label = lift . right . ThentosLabeled label

throwDBQ :: ThentosLabel -> DbError -> ThentosQuery a
throwDBQ label = lift . left . ThentosLabeled label

returnDBQ :: ThentosLabel -> a -> ThentosQuery a
returnDBQ label = lift . right . ThentosLabeled label


thentosPublic :: ThentosLabel
thentosPublic = True =%% False

thentosDenied :: ThentosLabel
thentosDenied = False =%% True

thentosLabeledPublic :: t -> ThentosLabeled t
thentosLabeledPublic = ThentosLabeled thentosPublic

thentosLabeledDenied :: t -> ThentosLabeled t
thentosLabeledDenied = ThentosLabeled thentosDenied

(=%%) :: (ToCNF a, ToCNF b) => a -> b -> ThentosLabel
(=%%) a b = ThentosLabel $ a %% b
infix 6 =%%

simpleThentosLabel :: ToCNF a => [a] -> ThentosLabel
simpleThentosLabel = ThentosLabel . simpleLabel

simpleLabel :: ToCNF a => [a] -> DCLabel
simpleLabel (map toCNF -> credentials) = case credentials of
    []     -> False %% True
    (x:xs) -> foldl' (/\) x xs %% foldl' (\/) x xs


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
