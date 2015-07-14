{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveFunctor               #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE InstanceSigs                #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE PackageImports              #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE UndecidableInstances        #-}

module Thentos.Action.Core
where

import Control.Applicative (Applicative, (<$>))
import Control.Concurrent (MVar, modifyMVar)
import Control.Exception (Exception, SomeException, throwIO, catch, ErrorCall(..))
import Control.Lens ((^.))
import Control.Monad.Except (MonadError, throwError, catchError)
import Control.Monad.Reader (ReaderT(ReaderT), MonadReader, runReaderT, ask)
import Control.Monad.Trans.Either (EitherT(EitherT), eitherT)
import "cryptonite" Crypto.Random (ChaChaDRG, DRG(randomBytesGenerate))
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

import System.Log (Priority(DEBUG, CRITICAL))

import qualified Data.Set as Set
import qualified Data.Thyme as Thyme

import LIO.Missing
import System.Log.Missing (logger)
import Thentos.Config
import Thentos.Smtp
import Thentos.Transaction
import Thentos.Types
import Thentos.Util


-- * types

newtype ActionState db =
    ActionState
      { fromActionState :: (AcidState db, MVar ChaChaDRG, ThentosConfig)
      }
  deriving (Typeable, Generic)

newtype Action db a =
    Action
      { fromAction :: ReaderT (ActionState db) (EitherT (ThentosError db) (LIO DCLabel)) a
      }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader (ActionState db)
           , MonadError (ThentosError db)
           , Typeable
           , Generic
           )

-- | Errors known by 'runActionE', 'runAction', ....
--
-- The 'MonadError' instance of newtype 'Action' lets you throw and catch errors from *within* the
-- 'Action', i.e., at construction time).  These are errors are handled in the 'ActionErrorThentos'
-- constructor.  Label errors and other (possibly async) exceptions are caught (if possible) in
-- 'runActionE' and its friends and maintained in other 'ActionError' constructors.
data ActionError db =
    ActionErrorThentos (ThentosError db)
  | ActionErrorAnyLabel AnyLabelError
  | ActionErrorUnknown SomeException

deriving instance Show (ActionError DB)
deriving instance Typeable ActionError

instance (db `Extends` DB, Show (ActionError db)) => Exception (ActionError db)

asDBActionError :: (db `Extends` DB) => ActionError DB -> ActionError db
asDBActionError (ActionErrorThentos e)  = ActionErrorThentos $ asDBThentosError e
asDBActionError (ActionErrorAnyLabel e) = ActionErrorAnyLabel e
asDBActionError (ActionErrorUnknown e)  = ActionErrorUnknown e


instance MonadLIO DCLabel (Action db) where
    liftLIO lio = Action . ReaderT $ \ _ -> EitherT (Right <$> lio)


-- * running actions

-- | Call 'runActionE' and throw 'Left' values.
runAction :: forall db a . (Exception (ActionError db)) => ActionState db -> Action db a -> IO a
runAction state action = runActionE state action >>= either throwIO return

runActionWithPrivs :: (ToCNF cnf, Exception (ActionError db)) =>
    [cnf] -> ActionState db -> Action db a -> IO a
runActionWithPrivs ars state action = runActionWithPrivsE ars state action >>= either throwIO return

runActionWithClearance :: Exception (ActionError db) =>
    DCLabel -> ActionState db -> Action db a -> IO a
runActionWithClearance label state action = runActionWithClearanceE label state action >>= either throwIO return

runActionAsAgent :: (db `Extends` DB, Exception (ActionError db)) =>
    Agent -> ActionState db -> Action db a -> IO a
runActionAsAgent agent state action = runActionAsAgentE agent state action >>= either throwIO return

runActionInThentosSession :: (db `Extends` DB, Exception (ActionError db)) =>
    ThentosSessionToken -> ActionState db -> Action db a -> IO a
runActionInThentosSession tok state action = runActionInThentosSessionE tok state action >>= either throwIO return

-- | Call an action with no access rights.  Catch all errors.  Initial LIO state is not
-- `dcDefaultState`, but @LIOState dcBottom dcBottom@: Only actions that require no clearance can be
-- executed, and the label has not been guarded by any action yet.
runActionE :: forall a db . ActionState db -> Action db a -> IO (Either (ActionError db) a)
runActionE state action = catchUnknown
  where
    inner :: IO (Either (ThentosError db) a)
    inner = (`evalLIO` LIOState dcBottom dcBottom)
          . eitherT (return . Left) (return . Right)
          $ fromAction action `runReaderT` state

    catchAnyLabelError :: IO (Either (ActionError db) a)
    catchAnyLabelError = (fmapL ActionErrorThentos <$> inner) `catch` (return . Left . ActionErrorAnyLabel)

    catchUnknown :: IO (Either (ActionError db) a)
    catchUnknown = catchAnyLabelError `catch` (return . Left . ActionErrorUnknown)

runActionWithPrivsE :: ToCNF cnf => [cnf] -> ActionState db -> Action db a -> IO (Either (ActionError db) a)
runActionWithPrivsE ars state = runActionE state . (grantAccessRights'P ars >>)

runActionWithClearanceE :: DCLabel -> ActionState db -> Action db a -> IO (Either (ActionError db) a)
runActionWithClearanceE label state = runActionE state . ((liftLIO $ setClearanceP (PrivTCB cFalse) label) >>)

runActionAsAgentE :: (db `Extends` DB) =>
    Agent -> ActionState db -> Action db a -> IO (Either (ActionError db) a)
runActionAsAgentE agent state = runActionE state . ((accessRightsByAgent'P agent >>= grantAccessRights'P) >>)

runActionInThentosSessionE :: (db `Extends` DB, Exception (ActionError db)) =>
    ThentosSessionToken -> ActionState db -> Action db a -> IO (Either (ActionError db) a)
runActionInThentosSessionE tok state = runActionE state . ((accessRightsByThentosSession'P tok >>= grantAccessRights'P) >>)

-- | Run an action followed by a second action. The second action is run
-- even if the first one throws an error.
finally :: forall a b db . Action db a -> Action db b -> Action db a
finally a sequel = do
    r <- catchError a (\e -> sequel >> throwError e)
    _ <- sequel
    return r


-- * labels, privileges and access rights.

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
grantAccessRights'P :: ToCNF cnf => [cnf] -> Action db ()
grantAccessRights'P ars = liftLIO $ setClearanceP (PrivTCB cFalse) c
  where
    c :: DCLabel
    c = foldl' lub dcBottom [ ar %% ar | ar <- ars ]

-- | Unravel role hierarchy stored under 'Agent' and construct a 'DCLabel'.  Termination is
-- guaranteed by the fact that the roles of the agent have been serialized in acid-state, and thus
-- are finite and cycle-free.
accessRightsByAgent'P :: (db `Extends` DB) => Agent -> Action db [CNF]
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

accessRightsByThentosSession'P :: (db `Extends` DB, Exception (ActionError db)) => ThentosSessionToken -> Action db [CNF]
accessRightsByThentosSession'P tok = do
    now <- getCurrentTime'P
    (_, session) <- update'P (LookupThentosSession now tok)
    accessRightsByAgent'P $ session ^. thSessAgent


-- * TCB business

-- | Call 'update'' on the 'ActionState' and re-throw the exception that has been turned into an
-- 'Either' on the border between acid-state and the real world.
update'P :: forall event db v
          . ( UpdateEvent event
            , EventState event ~ db
            , EventResult event ~ Either (ThentosError db) v
            , Exception (ActionError db)
            ) => event -> Action db v
update'P e = do
    ActionState (st, _, _) <- Action ask
    result <- liftLIO . ioTCB $ update' st e
    either throwError return result

-- | See 'update'P'.
query'P :: ( QueryEvent event
           , EventState event ~ db
           , EventResult event ~ Either (ThentosError db) v
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
    let f :: ChaChaDRG -> (ChaChaDRG, SBS)
        f r = case randomBytesGenerate i r of (output, r') -> (r', output)
    ActionState (_, mr, _) <- Action ask
    liftLIO . ioTCB . modifyMVar mr $ return . f

makeUserFromFormData'P :: UserFormData -> Action db User
makeUserFromFormData'P = liftLIO . ioTCB . makeUserFromFormData

hashUserPass'P :: UserPass -> Action db (HashedSecret UserPass)
hashUserPass'P = liftLIO . ioTCB . hashUserPass

hashServiceKey'P :: ServiceKey -> Action db (HashedSecret ServiceKey)
hashServiceKey'P = liftLIO . ioTCB . hashServiceKey

sendMail'P :: SmtpConfig -> Maybe UserName -> UserEmail -> ST -> ST -> Action db ()
sendMail'P config mName address subject msg = liftLIO . ioTCB $ do
    result <- sendMail config mName address subject msg
    case result of
        Right () -> return ()
        Left (SendmailError s) -> do
            logger CRITICAL $ "error sending mail: " ++ s
            throwIO $ ErrorCall "error sending email"

logger'P :: Priority -> String -> Action db ()
logger'P prio = liftLIO . ioTCB . logger prio

-- | (This type signature could be greatly simplified, but that would also make it less explanatory.)
logIfError'P :: forall m l e v db
    . (m ~ Action db, Monad m, MonadLIO l m, MonadError e m, Show e, Show (ThentosError db))
    => m v -> m v
logIfError'P = (`catchError` f)
  where
    f e = do
        logger'P DEBUG $ "*** error: " ++ show e
        throwError e
