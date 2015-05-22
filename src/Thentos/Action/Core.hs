{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PackageImports         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE ViewPatterns           #-}

module Thentos.Action.Core
where

import Control.Applicative (Applicative, (<*>), (<$>), pure)
import Control.Concurrent (MVar, modifyMVar)
import Control.Exception (Exception, SomeException, throwIO, catch)
import Control.Lens ((^.))
import Control.Monad.Except (MonadError, throwError, catchError)
import Control.Monad.Reader (ReaderT(ReaderT), MonadReader, runReaderT, ask, local)
import Control.Monad.Trans.Either (EitherT(EitherT), eitherT)
import "crypto-random" Crypto.Random (SystemRNG, cprgGenerate)
import Data.Acid (AcidState, UpdateEvent, QueryEvent, EventState, EventResult)
import Data.Acid.Advanced (query', update')
import Data.EitherR (fmapL)
import Data.List (foldl')
import Data.String.Conversions (ST, SBS)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import LIO.Core (MonadLIO, LIO, LIOState(LIOState), liftLIO, evalLIO, setClearanceP)
import LIO.Label (lub)
import LIO.DCLabel (CNF, ToCNF, DCLabel, (%%), toCNF, cFalse)
import LIO.Error (AnyLabelError)
import LIO.TCB (Priv(PrivTCB), ioTCB)

import System.Log (Priority(DEBUG))

import qualified Data.Set as Set
import qualified Data.Thyme as Thyme

import System.Log.Missing (logger)
import Thentos.Config
import Thentos.Smtp
import Thentos.Transaction
import Thentos.Types
import Thentos.Util


-- * types

newtype ActionState db = ActionState { fromActionState :: (AcidState db, MVar SystemRNG, ThentosConfig) }
  deriving (Typeable, Generic)

newtype Action db a = Action { fromAction :: ReaderT (ActionState db) (EitherT ThentosError (LIO DCLabel)) a }
  deriving (Typeable, Generic)

instance Functor (Action db) where
    fmap f (Action a) = Action $ fmap f a

instance Applicative (Action db) where
    pure = Action . pure
    Action a <*> Action b = Action $ a <*> b

instance Monad (Action db) where
    return = Action . return
    (Action a) >>= f = Action $ a >>= fromAction . f

instance MonadReader (ActionState db) (Action db) where
    ask = Action ask
    local f (Action a) = Action $ local f a

-- | FIXME: all exceptions that occur inside 'LIO' currently go uncaught until execution reaches
-- 'runAction' or 'runActionE'.  should errors caught there actually be handled here already, so we
-- can process them inside 'Action'?
instance MonadError ThentosError (Action db) where
    throwError :: ThentosError -> Action db a
    throwError = Action . throwError

    catchError :: Action db a -> (ThentosError -> Action db a) -> Action db a
    catchError (Action a) handler = Action $ catchError a (fromAction . handler)

instance MonadLIO DCLabel (Action db) where
    liftLIO lio = Action . ReaderT $ \ _ -> EitherT (Right <$> lio)


data ActionError =
    ActionErrorThentos ThentosError
  | ActionErrorAnyLabel AnyLabelError
  | ActionErrorUnknown SomeException
  deriving (Show, Typeable, Generic)

instance Exception ActionError


-- * running actions

-- | Call 'runActionE' and throw 'Left' values.
runAction :: ActionState db -> Action db a -> IO a
runAction state action = runActionE state action >>= either throwIO return

runActionWithPrivs ::  ToCNF cnf => [cnf] -> ActionState DB -> Action DB a -> IO a
runActionWithPrivs ars state action = runActionWithPrivsE ars state action >>= either throwIO return

runActionAsAgent :: Agent -> ActionState DB -> Action DB a -> IO a
runActionAsAgent agent state action = runActionAsAgentE agent state action >>= either throwIO return

runActionInThentosSession :: ThentosSessionToken -> ActionState DB -> Action DB a -> IO a
runActionInThentosSession tok state action = runActionInThentosSessionE tok state action >>= either throwIO return

-- | Call an action with no access rights.  Catch all errors.  Initial LIO state is not
-- `dcDefaultState`, but @LIOState dcBottom dcBottom@: Only actions that require no clearance can be
-- executed, and the label has not been guarded by any action yet.
runActionE :: forall a db . ActionState db -> Action db a -> IO (Either ActionError a)
runActionE state action = catchUnknown
  where
    inner :: IO (Either ThentosError a)
    inner = (`evalLIO` LIOState dcBottom dcBottom)
          . liftLIO
          . eitherT (return . Left) (return . Right)
          $ fromAction action `runReaderT` state

    catchAnyLabelError :: IO (Either ActionError a)
    catchAnyLabelError = (fmapL ActionErrorThentos <$> inner) `catch` (return . Left . ActionErrorAnyLabel)

    catchUnknown :: IO (Either ActionError a)
    catchUnknown = catchAnyLabelError `catch` (return . Left . ActionErrorUnknown)

runActionWithPrivsE :: ToCNF cnf => [cnf] -> ActionState DB -> Action DB a -> IO (Either ActionError a)
runActionWithPrivsE ars state = runActionE state . (grantAccessRights'P ars >>)

runActionAsAgentE :: Agent -> ActionState DB -> Action DB a -> IO (Either ActionError a)
runActionAsAgentE agent state = runActionE state . ((accessRightsByAgent'P agent >>= grantAccessRights'P) >>)

runActionInThentosSessionE :: ThentosSessionToken -> ActionState DB -> Action DB a -> IO (Either ActionError a)
runActionInThentosSessionE tok state = runActionE state . ((accessRightsByThentosSession'P tok >>= grantAccessRights'P) >>)


-- * labels, privileges and access rights.


-- | FIXME: move to LIO.Missing; make pull request
dcBottom :: DCLabel
dcBottom = True %% False

-- | FIXME: move to LIO.Missing; make pull request
dcTop :: DCLabel
dcTop = False %% True


-- | In order to execute an 'Action', certain access rights need to be granted.  A set of access
-- rights is a list of 'ToCNF' instances that are used to update the current clearance in the
-- 'LIOState' in the 'LIO' monad underlying 'Action'.
--
-- To execute an 'Action' with the access rights of 'UserId' @u@ and 'BasicRole' @r@:
--
-- >>> grantAccessRights'P [toCNF u, toCNF r]
--
-- Or, to grant just @r@:
--
-- >>> grantAccessRights'P [r]
--
-- Adding more access rights must increase access, so for a list @ars@ of access rights, the
-- constructed clearance level @c@ must satisfy:
--
-- >>> and [ (ar %% ar) `canFlowTo` c | ar <- ars ]
--
-- Therefore, @c@ is defined as the least upper bound (join) of the labels constructed from
-- individual access rights:
--
-- >>> c = foldl' (lub) dcBottom [ ar %% ar | ar <- ars ]
grantAccessRights'P :: ToCNF cnf => [cnf] -> Action DB ()
grantAccessRights'P ars = liftLIO $ setClearanceP (PrivTCB cFalse) c
  where
    c :: DCLabel
    c = foldl' lub dcBottom [ ar %% ar | ar <- ars ]

-- | Unravel role hierarchy stored under 'Agent' and construct a 'DCLabel'.  Termination is
-- guaranteed by the fact that the roles of the agent have been serialized in acid-state, and thus
-- are finite and cycle-free.
accessRightsByAgent'P :: Agent -> Action DB [CNF]
accessRightsByAgent'P agent = Set.toList . makeAccessRights <$> query'P (AgentRoles agent)
  where
    makeAccessRights :: Set.Set Role -> Set.Set CNF
    makeAccessRights roles = Set.fold Set.insert agent' roles'
      where
        agent' = Set.singleton $ toCNF agent
        roles' = Set.map toCNF $ flatten roles

    flatten :: Set.Set Role -> Set.Set RoleBasic
    flatten = Set.fold (flip f) Set.empty
      where
        f :: Set.Set RoleBasic -> Role -> Set.Set RoleBasic
        f acc (Roles rs) = foldl' f acc rs
        f acc (RoleBasic b) = Set.insert b acc

accessRightsByThentosSession'P :: ThentosSessionToken -> Action DB [CNF]
accessRightsByThentosSession'P tok = do
    now <- getCurrentTime'P
    (_, session) <- update'P (LookupThentosSession now tok)
    accessRightsByAgent'P $ session ^. thSessAgent


-- * TCB business

-- | Call 'update'' on the 'ActionState' and re-throw the exception that has been turned into an
-- 'Either' on the border between acid-state and the real world.
update'P :: ( UpdateEvent event
            , EventState event ~ db
            , EventResult event ~ Either ThentosError v
            ) => event -> Action db v
update'P e = do
    ActionState (st, _, _) <- Action ask
    result <- liftLIO . ioTCB $ update' st e
    either throwError return result

-- | See 'updateA'.
query'P :: ( QueryEvent event
           , EventState event ~ db
           , EventResult event ~ Either ThentosError v
           ) => event -> Action db v
query'P e = do
    (ActionState (st, _, _)) <- Action ask
    result <- liftLIO . ioTCB $ query' st e
    either throwError return result

getConfig'P :: Action db ThentosConfig
getConfig'P = (\ (ActionState (_, _, c)) -> c) <$> Action ask

getCurrentTime'P :: Action db Timestamp
getCurrentTime'P = Timestamp <$> liftLIO (ioTCB Thyme.getCurrentTime)

-- | A relative of 'cprgGenerate' from crypto-random that lives in
-- 'Action'.
genRandomBytes'P :: Int -> Action db SBS
genRandomBytes'P i = do
    let f :: SystemRNG -> (SystemRNG, SBS)
        f r = case cprgGenerate i r of (output, r') -> (r', output)
    ActionState (_, mr, _) <- Action ask
    liftLIO . ioTCB . modifyMVar mr $ return . f

makeUserFromFormData'P :: UserFormData -> Action db User
makeUserFromFormData'P = liftLIO . ioTCB . makeUserFromFormData

hashUserPass'P :: UserPass -> Action db (HashedSecret UserPass)
hashUserPass'P = liftLIO . ioTCB . hashUserPass

hashServiceKey'P :: ServiceKey -> Action db (HashedSecret ServiceKey)
hashServiceKey'P = liftLIO . ioTCB . hashServiceKey

sendMail'P :: SmtpConfig -> Maybe UserName -> UserEmail -> ST -> ST -> Action db ()
sendMail'P config mName address subject = liftLIO . ioTCB . sendMail config mName address subject

logger'P :: Priority -> String -> Action db ()
logger'P prio = liftLIO . ioTCB . logger prio

-- | (This type signature could be greatly simplified, but that would also make it less explanatory.)
logIfError'P :: forall m l e v db . (m ~ Action db, Monad m, MonadLIO l m, MonadError e m, Show e) => m v -> m v
logIfError'P = (`catchError` f)
  where
    f e = do
        logger'P DEBUG $ "*** error: " ++ show e
        throwError e