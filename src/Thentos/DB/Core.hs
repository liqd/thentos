{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

module Thentos.DB.Core
  ( DbError(..)
  , ThentosClearance(..)
  , ThentosUpdate, ThentosUpdate'
  , runThentosUpdate
  , ThentosQuery, ThentosQuery'
  , runThentosQuery
  , liftThentosQuery
  , showDbError
  , returnDb
  , throwDb
  , thentosPublic
  , thentosDenied
  , thentosLabeledPublic
  , thentosLabeledDenied
  , (=%%)
  , TLMode(TLRead, TLReadWrite)
  , makeThentosLabel
  , restrictThentosLabel
  , restrictThentosLabel'
  , makeThentosLabel1
  , makeThentosLabel2
  , makeThentosLabel3
  , makeThentosLabel4
  , makeThentosLabel5
  , createCheckpointLoop
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay, forkIO, ThreadId)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State (StateT(StateT), runStateT, get, put)
import Control.Monad.Trans.Either (EitherT(EitherT), right, runEitherT)
import Data.Acid (AcidState, Update, Query, createCheckpoint)
import Data.EitherR (throwT)
import Data.SafeCopy (SafeCopy, contain, putCopy, getCopy, safePut, safeGet)
import Data.Typeable (Typeable)
import LIO (canFlowTo)
import LIO.DCLabel (DCLabel(DCLabel), ToCNF, (%%), (/\), (\/), toCNF)
import Safe (readMay)
import System.Log.Logger (Priority(INFO))
import System.Log.Missing (logger)

import Thentos.Types


-- * types

-- FIXME: many of these aren't DB errors at all
data DbError =
      NoSuchUser
    | NoSuchPendingUserConfirmation
    | NoSuchService
    | NoSuchSession
    | OperationNotPossibleInServiceSession
    | ServiceAlreadyExists
    | UserEmailAlreadyExists
    | UserNameAlreadyExists
    | PermissionDenied ThentosClearance ThentosLabel
    | BadCredentials
    | BadAuthenticationHeaders
    | ProxyNotAvailable
    | MissingServiceHeader
    | ProxyNotConfiguredForService ServiceId
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

liftThentosQuery :: forall e a . ThentosQuery' e a -> ThentosUpdate' e a
liftThentosQuery thentosQuery = EitherT $ StateT $ \ state ->
    (, state) <$> runEitherT thentosQuery `runReaderT` state


-- | the type of this will change when servant has a better error type.
showDbError :: MonadIO m => DbError -> m (Int, String)
showDbError NoSuchUser                           = return (404, "user not found")
showDbError NoSuchPendingUserConfirmation        = return (404, "unconfirmed user not found")
showDbError NoSuchService                        = return (404, "service not found")
showDbError NoSuchSession                        = return (404, "session not found")
showDbError OperationNotPossibleInServiceSession = return (404, "operation not possible in service session")
showDbError ServiceAlreadyExists                 = return (403, "service already exists")
showDbError UserEmailAlreadyExists               = return (403, "email already in use")
showDbError UserNameAlreadyExists                = return (403, "user name already in use")
showDbError e@(PermissionDenied _ _)             = logger INFO (show e) >> return (401, "unauthorized")
showDbError e@BadCredentials                     = logger INFO (show e) >> return (401, "unauthorized")
showDbError BadAuthenticationHeaders             = return (400, "bad authentication headers")
showDbError ProxyNotAvailable                    = return (404, "proxying not activated")
showDbError MissingServiceHeader                 = return (404, "headers do not contain service id")
showDbError (ProxyNotConfiguredForService sid)   = return (404, "proxy not configured for service " ++ show sid)


-- | FIXME: generalize, so we can use this for both Update and Query.
-- (remove 'runThentosQuery' and 'ThentosQuery' when done.)
runThentosUpdate :: forall a . ThentosClearance -> ThentosUpdate a -> Update DB (Either DbError a)
runThentosUpdate clearance action = do
    state <- get
    case runIdentity $ runStateT (runEitherT action) state of
        (Left (ThentosLabeled label (err :: DbError)), _) ->
            checkClearance clearance label (return $ Left err)
        (Right (ThentosLabeled label result), state') ->
            checkClearance clearance label (put state' >> return (Right result))

runThentosQuery :: forall a . ThentosClearance -> ThentosQuery a -> Query DB (Either DbError a)
runThentosQuery clearance action = do
    state <- ask
    case runIdentity $ runReaderT (runEitherT action) state of
        Left (ThentosLabeled label (err :: DbError)) ->
            checkClearance clearance label (return $ Left err)
        Right (ThentosLabeled label result) ->
            checkClearance clearance label (return $ Right result)

checkClearance :: Monad m => ThentosClearance -> ThentosLabel -> m (Either DbError a) -> m (Either DbError a)
checkClearance clearance label result =
    if fromThentosLabel label `canFlowTo` fromThentosClearance clearance
        then result
        else return . Left $ PermissionDenied clearance label

returnDb :: Monad m => ThentosLabel -> a -> EitherT (ThentosLabeled e) m (ThentosLabeled a)
returnDb l a = right $ ThentosLabeled l a

throwDb :: Monad m => ThentosLabel -> DbError -> EitherT (ThentosLabeled DbError) m (ThentosLabeled a)
throwDb l e = throwT $ ThentosLabeled l e


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

data TLMode = TLRead | TLReadWrite
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | See also:
--
-- * 'makeClearance_' in "Thentos.DB.Protected" (not exported)
-- * test cases in @TestMain@.
makeThentosLabel :: ToCNF a => TLMode -> [a] -> ThentosLabel
makeThentosLabel _      [] = thentosDenied
makeThentosLabel tlMode (x:xs) = case tlMode of
      TLRead      -> s =%% True
      TLReadWrite -> s =%% i
  where
    s = foldr (\/) (toCNF x) (map toCNF xs)
    i = foldr (/\) (toCNF x) (map toCNF xs)

restrictThentosLabel :: ToCNF a => TLMode -> a -> ThentosLabel -> ThentosLabel
restrictThentosLabel TLRead      a (ThentosLabel (DCLabel s _)) = s \/ a =%% True
restrictThentosLabel TLReadWrite a (ThentosLabel (DCLabel s i)) = s \/ a =%% i /\ a

restrictThentosLabel' :: ToCNF a => TLMode -> Maybe a -> ThentosLabel -> ThentosLabel
restrictThentosLabel' _      Nothing = id
restrictThentosLabel' tlMode (Just a) = restrictThentosLabel tlMode a

makeThentosLabel1 :: ToCNF a => TLMode -> a -> ThentosLabel
makeThentosLabel1 tlMode a = makeThentosLabel tlMode [a]

makeThentosLabel2 :: (ToCNF a, ToCNF b) => TLMode -> a -> b -> ThentosLabel
makeThentosLabel2 tlMode a b = makeThentosLabel tlMode [toCNF a, toCNF b]

makeThentosLabel3 :: (ToCNF a, ToCNF b, ToCNF c) => TLMode -> a -> b -> c -> ThentosLabel
makeThentosLabel3 tlMode a b c = makeThentosLabel tlMode [toCNF a, toCNF b, toCNF c]

makeThentosLabel4 :: (ToCNF a, ToCNF b, ToCNF c, ToCNF d) => TLMode -> a -> b -> c -> d -> ThentosLabel
makeThentosLabel4 tlMode a b c d = makeThentosLabel tlMode [toCNF a, toCNF b, toCNF c, toCNF d]

makeThentosLabel5 :: (ToCNF a, ToCNF b, ToCNF c, ToCNF d, ToCNF e) => TLMode -> a -> b -> c -> d -> e -> ThentosLabel
makeThentosLabel5 tlMode a b c d e = makeThentosLabel tlMode [toCNF a, toCNF b, toCNF c, toCNF d, toCNF e]


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
