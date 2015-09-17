{-# LANGUAGE ConstraintKinds             #-}
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
import Data.Pool (Pool, withResource)
import Data.EitherR (fmapL)
import Data.List (foldl')
import Data.String.Conversions (ST, SBS)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import LIO.Core (MonadLIO, LIO, LIOState(LIOState), liftLIO, evalLIO, setClearanceP, taint,
                 guardWrite)
import LIO.Label (lub)
import LIO.DCLabel (CNF, ToCNF, DCLabel, (%%), toCNF, cFalse)
import LIO.Error (AnyLabelError)
import LIO.TCB (Priv(PrivTCB), ioTCB)
import Database.PostgreSQL.Simple (Connection)

import System.Log (Priority(DEBUG, CRITICAL))

import qualified Data.Thyme as Thyme

import LIO.Missing
import System.Log.Missing (logger)
import Thentos.Config
import Thentos.Smtp
import qualified Thentos.Transaction as T
import Thentos.Transaction.Core (ThentosQuery, runThentosQuery)
import Thentos.Types
import Thentos.Util


-- * types

newtype ActionState =
    ActionState
      { fromActionState :: (Pool Connection, MVar ChaChaDRG, ThentosConfig)
      }
  deriving (Typeable, Generic)

newtype Action e a =
    Action
      { fromAction :: ReaderT ActionState (EitherT (ThentosError e) (LIO DCLabel)) a
      }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader ActionState
           , MonadError (ThentosError e)
           , Typeable
           , Generic
           )

-- | Errors known by 'runActionE', 'runAction', ....
--
-- The 'MonadError' instance of newtype 'Action' lets you throw and catch errors from *within* the
-- 'Action', i.e., at construction time).  These are errors are handled in the 'ActionErrorThentos'
-- constructor.  Label errors and other (possibly async) exceptions are caught (if possible) in
-- 'runActionE' and its friends and maintained in other 'ActionError' constructors.
data ActionError e =
    ActionErrorThentos (ThentosError e)
  | ActionErrorAnyLabel AnyLabelError
  | ActionErrorUnknown SomeException
  deriving ( Show
           , Typeable
           )

instance (Show e, Typeable e) => Exception (ActionError e)

instance MonadLIO DCLabel (Action e) where
    liftLIO lio = Action . ReaderT $ \ _ -> EitherT (Right <$> lio)


-- * running actions

-- | Call 'runActionE' and throw 'Left' values.
runAction :: (Show e, Typeable e) => ActionState -> Action e a -> IO a
runAction state action = runActionE state action >>= either throwIO return

runActionWithPrivs :: (ToCNF cnf, Show e, Typeable e) =>
    [cnf] -> ActionState -> Action e a -> IO a
runActionWithPrivs ars state action = runActionWithPrivsE ars state action >>= either throwIO return

runActionWithClearance :: (Show e, Typeable e) =>
    DCLabel -> ActionState -> Action e a -> IO a
runActionWithClearance label state action = runActionWithClearanceE label state action >>= either throwIO return

runActionAsAgent :: (Show e, Typeable e) =>
    Agent -> ActionState -> Action e a -> IO a
runActionAsAgent agent state action = runActionAsAgentE agent state action >>= either throwIO return

runActionInThentosSession :: (Show e, Typeable e) =>
    ThentosSessionToken -> ActionState -> Action e a -> IO a
runActionInThentosSession tok state action = runActionInThentosSessionE tok state action >>= either throwIO return

-- | Call an action with no access rights.  Catch all errors.  Initial LIO state is not
-- `dcDefaultState`, but @LIOState dcBottom dcBottom@: Only actions that require no clearance can be
-- executed, and the label has not been guarded by any action yet.
runActionE :: (Show e, Typeable e) =>
    ActionState -> Action e a -> IO (Either (ActionError e) a)
runActionE state action = catchUnknown
  where
    inner = (`evalLIO` LIOState dcBottom dcBottom)
          . eitherT (return . Left) (return . Right)
          $ fromAction action `runReaderT` state
    catchAnyLabelError = (fmapL ActionErrorThentos <$> inner) `catch` (return . Left . ActionErrorAnyLabel)
    catchUnknown = catchAnyLabelError `catch` (return . Left . ActionErrorUnknown)

runActionWithPrivsE :: (ToCNF cnf, Show e, Typeable e) =>
    [cnf] -> ActionState -> Action e a -> IO (Either (ActionError e) a)
runActionWithPrivsE ars state = runActionE state . (grantAccessRights'P ars >>)

runActionWithClearanceE :: (Show e, Typeable e) =>
    DCLabel -> ActionState -> Action e a -> IO (Either (ActionError e) a)
runActionWithClearanceE label state = runActionE state . ((liftLIO $ setClearanceP (PrivTCB cFalse) label) >>)

runActionAsAgentE :: (Show e, Typeable e) =>
    Agent -> ActionState -> Action e a -> IO (Either (ActionError e) a)
runActionAsAgentE agent state = runActionE state . ((accessRightsByAgent'P agent >>= grantAccessRights'P) >>)

runActionInThentosSessionE :: (Show e, Typeable e) =>
    ThentosSessionToken -> ActionState -> Action e a -> IO (Either (ActionError e) a)
runActionInThentosSessionE tok state = runActionE state . ((accessRightsByThentosSession'P tok >>= grantAccessRights'P) >>)

-- | Run an action followed by a second action. The second action is run
-- even if the first one throws an error.
finally :: forall e a b . Action e a -> Action e b -> Action e a
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
grantAccessRights'P :: ToCNF cnf => [cnf] -> Action e ()
grantAccessRights'P ars = liftLIO $ setClearanceP (PrivTCB cFalse) c
  where
    c :: DCLabel
    c = foldl' lub dcBottom [ ar %% ar | ar <- ars ]

-- | Construct a 'DCLabel' from agent's roles.
accessRightsByAgent'P :: Agent -> Action e [CNF]
accessRightsByAgent'P agent = makeAccessRights <$> query'P (T.agentRoles agent)
  where
    makeAccessRights :: [Role] -> [CNF]
    makeAccessRights roles = toCNF agent : map toCNF roles

accessRightsByThentosSession'P :: ThentosSessionToken -> Action e [CNF]
accessRightsByThentosSession'P tok = do
    (_, session) <- query'P (T.lookupThentosSession tok)
    accessRightsByAgent'P $ session ^. thSessAgent


-- * TCB business

query'P :: ThentosQuery e v -> Action e v
query'P u = do
    ActionState (connPool, _, _) <- Action ask
    result <- liftLIO . ioTCB . withResource connPool $ \conn -> runThentosQuery conn u
    either throwError return result

getConfig'P :: Action e ThentosConfig
getConfig'P = (\ (ActionState (_, _, c)) -> c) <$> Action ask

getCurrentTime'P :: Action e Timestamp
getCurrentTime'P = Timestamp <$> liftLIO (ioTCB Thyme.getCurrentTime)

-- | A relative of 'cprgGenerate' from crypto-random that lives in
-- 'Action'.
genRandomBytes'P :: Int -> Action e SBS
genRandomBytes'P i = do
    let f :: ChaChaDRG -> (ChaChaDRG, SBS)
        f r = case randomBytesGenerate i r of (output, r') -> (r', output)
    ActionState (_, mr, _) <- Action ask
    liftLIO . ioTCB . modifyMVar mr $ return . f

makeUserFromFormData'P :: UserFormData -> Action e User
makeUserFromFormData'P = liftLIO . ioTCB . makeUserFromFormData

hashUserPass'P :: UserPass -> Action e (HashedSecret UserPass)
hashUserPass'P = liftLIO . ioTCB . hashUserPass

hashServiceKey'P :: ServiceKey -> Action e (HashedSecret ServiceKey)
hashServiceKey'P = liftLIO . ioTCB . hashServiceKey

sendMail'P :: SmtpConfig -> Maybe UserName -> UserEmail -> ST -> ST -> Action e ()
sendMail'P config mName address subject msg = liftLIO . ioTCB $ do
    result <- sendMail config mName address subject msg
    case result of
        Right () -> return ()
        Left (SendmailError s) -> do
            logger CRITICAL $ "error sending mail: " ++ s
            throwIO $ ErrorCall "error sending email"

logger'P :: Priority -> String -> Action e ()
logger'P prio = liftLIO . ioTCB . logger prio

-- | (This type signature could be greatly simplified, but that would also make it less explanatory.)
logIfError'P :: forall m l e v f
    . (m ~ Action f, Monad m, MonadLIO l m, MonadError e m, Show e, Show f)
    => m v -> m v
logIfError'P = (`catchError` f)
  where
    f e = do
        logger'P DEBUG $ "*** error: " ++ show e
        throwError e


-- * better label errors

-- | Call 'taint', but log a more informative error in case of fail.
taintMsg :: String -> DCLabel -> Action e ()
taintMsg msg l = do
    tryTaint l (return ()) $ \ (e :: AnyLabelError) -> do
        logger'P DEBUG $ ("taintMsg:\n    " ++ msg ++ "\n    " ++ show e)
        liftLIO $ taint l

guardWriteMsg :: String -> DCLabel -> Action e ()
guardWriteMsg msg l = do
    tryGuardWrite l (return ()) $ \ (e :: AnyLabelError) -> do
        logger'P DEBUG $ "guardWrite:\n    " ++ msg ++ "\n    " ++ show e
        liftLIO $ guardWrite l
