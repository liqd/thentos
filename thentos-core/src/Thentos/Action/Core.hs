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
import Data.EitherR (fmapL)
import Data.List (foldl')
import Data.String.Conversions (ST, SBS)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import LIO.Core (MonadLIO, LIO, LIOState(LIOState), liftLIO, evalLIO, setClearanceP, taint, guardWrite)
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
import qualified Thentos.Transaction as T
import Thentos.Transaction.Core (ThentosQuery(..), ThentosUpdate(..))
import Thentos.Types
import Thentos.Util


-- * types

newtype ActionState =
    ActionState
      { fromActionState :: ((), MVar ChaChaDRG, ThentosConfig)
      }
  deriving (Typeable, Generic)

newtype Action a =
    Action
      { fromAction :: ReaderT ActionState (EitherT ThentosError (LIO DCLabel)) a
      }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader ActionState
           , MonadError ThentosError
           , Typeable
           , Generic
           )

-- | Errors known by 'runActionE', 'runAction', ....
--
-- The 'MonadError' instance of newtype 'Action' lets you throw and catch errors from *within* the
-- 'Action', i.e., at construction time).  These are errors are handled in the 'ActionErrorThentos'
-- constructor.  Label errors and other (possibly async) exceptions are caught (if possible) in
-- 'runActionE' and its friends and maintained in other 'ActionError' constructors.
data ActionError =
    ActionErrorThentos ThentosError
  | ActionErrorAnyLabel AnyLabelError
  | ActionErrorUnknown SomeException

deriving instance Show ActionError
deriving instance Typeable ActionError
instance Exception ActionError

instance MonadLIO DCLabel Action where
    liftLIO lio = Action . ReaderT $ \ _ -> EitherT (Right <$> lio)


-- * running actions

-- | Call 'runActionE' and throw 'Left' values.
runAction :: ActionState -> Action a -> IO a
runAction state action = runActionE state action >>= either throwIO return

runActionWithPrivs :: (ToCNF cnf) =>
    [cnf] -> ActionState -> Action a -> IO a
runActionWithPrivs ars state action = runActionWithPrivsE ars state action >>= either throwIO return

runActionWithClearance ::
    DCLabel -> ActionState -> Action a -> IO a
runActionWithClearance label state action = runActionWithClearanceE label state action >>= either throwIO return

runActionAsAgent ::
    Agent -> ActionState -> Action a -> IO a
runActionAsAgent agent state action = runActionAsAgentE agent state action >>= either throwIO return

runActionInThentosSession ::
    ThentosSessionToken -> ActionState -> Action a -> IO a
runActionInThentosSession tok state action = runActionInThentosSessionE tok state action >>= either throwIO return

-- | Call an action with no access rights.  Catch all errors.  Initial LIO state is not
-- `dcDefaultState`, but @LIOState dcBottom dcBottom@: Only actions that require no clearance can be
-- executed, and the label has not been guarded by any action yet.
runActionE ::
    ActionState -> Action a -> IO (Either ActionError a)
runActionE state action = catchUnknown
  where
--    inner :: IO (Either ThentosError a)
    inner = (`evalLIO` LIOState dcBottom dcBottom)
          . eitherT (return . Left) (return . Right)
          $ fromAction action `runReaderT` state

--    catchAnyLabelError :: IO (Either ActionError a)
    catchAnyLabelError = (fmapL ActionErrorThentos <$> inner) `catch` (return . Left . ActionErrorAnyLabel)

--    catchUnknown :: IO (Either ActionError a)
    catchUnknown = catchAnyLabelError `catch` (return . Left . ActionErrorUnknown)

runActionWithPrivsE :: ToCNF cnf =>
    [cnf] -> ActionState -> Action a -> IO (Either ActionError a)
runActionWithPrivsE ars state = runActionE state . (grantAccessRights'P ars >>)

runActionWithClearanceE ::
    DCLabel -> ActionState -> Action a -> IO (Either ActionError a)
runActionWithClearanceE label state = runActionE state . ((liftLIO $ setClearanceP (PrivTCB cFalse) label) >>)

runActionAsAgentE ::
    Agent -> ActionState -> Action a -> IO (Either ActionError a)
runActionAsAgentE agent state = runActionE state . ((accessRightsByAgent'P agent >>= grantAccessRights'P) >>)

runActionInThentosSessionE ::
    ThentosSessionToken -> ActionState -> Action a -> IO (Either ActionError a)
runActionInThentosSessionE tok state = runActionE state . ((accessRightsByThentosSession'P tok >>= grantAccessRights'P) >>)

-- | Run an action followed by a second action. The second action is run
-- even if the first one throws an error.
finally :: forall a b . Action a -> Action b -> Action a
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
grantAccessRights'P :: ToCNF cnf => [cnf] -> Action ()
grantAccessRights'P ars = liftLIO $ setClearanceP (PrivTCB cFalse) c
  where
    c :: DCLabel
    c = foldl' lub dcBottom [ ar %% ar | ar <- ars ]

-- | Unravel role hierarchy stored under 'Agent' and construct a 'DCLabel'.  Termination is
-- guaranteed by the fact that the roles of the agent have been serialized in acid-state, and thus
-- are finite and cycle-free.
accessRightsByAgent'P :: Agent -> Action [CNF]
accessRightsByAgent'P agent = Set.toList . makeAccessRights <$> query'P (T.agentRoles agent)
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

accessRightsByThentosSession'P :: ThentosSessionToken -> Action [CNF]
accessRightsByThentosSession'P tok = do
    now <- getCurrentTime'P
    (_, session) <- update'P (T.lookupThentosSession now tok)
    accessRightsByAgent'P $ session ^. thSessAgent


-- * TCB business

-- | Call 'update'' on the 'ActionState' and re-throw the exception that has been turned into an
-- 'Either' on the border between acid-state and the real world.
update'P :: ThentosUpdate v -> Action v
update'P = undefined

-- | See 'update'P'.
query'P :: ThentosQuery v -> Action v
query'P = undefined

getConfig'P :: Action ThentosConfig
getConfig'P = (\ (ActionState (_, _, c)) -> c) <$> Action ask

getCurrentTime'P :: Action Timestamp
getCurrentTime'P = Timestamp <$> liftLIO (ioTCB Thyme.getCurrentTime)

-- | A relative of 'cprgGenerate' from crypto-random that lives in
-- 'Action'.
genRandomBytes'P :: Int -> Action SBS
genRandomBytes'P i = do
    let f :: ChaChaDRG -> (ChaChaDRG, SBS)
        f r = case randomBytesGenerate i r of (output, r') -> (r', output)
    ActionState (_, mr, _) <- Action ask
    liftLIO . ioTCB . modifyMVar mr $ return . f

makeUserFromFormData'P :: UserFormData -> Action User
makeUserFromFormData'P = liftLIO . ioTCB . makeUserFromFormData

hashUserPass'P :: UserPass -> Action (HashedSecret UserPass)
hashUserPass'P = liftLIO . ioTCB . hashUserPass

hashServiceKey'P :: ServiceKey -> Action (HashedSecret ServiceKey)
hashServiceKey'P = liftLIO . ioTCB . hashServiceKey

sendMail'P :: SmtpConfig -> Maybe UserName -> UserEmail -> ST -> ST -> Action ()
sendMail'P config mName address subject msg = liftLIO . ioTCB $ do
    result <- sendMail config mName address subject msg
    case result of
        Right () -> return ()
        Left (SendmailError s) -> do
            logger CRITICAL $ "error sending mail: " ++ s
            throwIO $ ErrorCall "error sending email"

logger'P :: Priority -> String -> Action ()
logger'P prio = liftLIO . ioTCB . logger prio

-- | (This type signature could be greatly simplified, but that would also make it less explanatory.)
logIfError'P :: forall m l e v
    . (m ~ Action, Monad m, MonadLIO l m, MonadError e m, Show e, Show ThentosError)
    => m v -> m v
logIfError'P = (`catchError` f)
  where
    f e = do
        logger'P DEBUG $ "*** error: " ++ show e
        throwError e


-- * better label errors

-- | Call 'taint', but log a more informative error in case of fail.
taintMsg :: String -> DCLabel -> Action ()
taintMsg msg l = do
    tryTaint l (return ()) $ \ (e :: AnyLabelError) -> do
        logger'P DEBUG $ ("taintMsg:\n    " ++ msg ++ "\n    " ++ show e)
        liftLIO $ taint l

guardWriteMsg :: String -> DCLabel -> Action ()
guardWriteMsg msg l = do
    tryGuardWrite l (return ()) $ \ (e :: AnyLabelError) -> do
        logger'P DEBUG $ "guardWrite:\n    " ++ msg ++ "\n    " ++ show e
        liftLIO $ guardWrite l
