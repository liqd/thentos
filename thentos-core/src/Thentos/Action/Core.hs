{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TypeFamilies                #-}

module Thentos.Action.Core
where

import Control.Arrow (first)
import Control.Exception (Exception, throwIO, catch)
import Control.Lens ((^.))
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (runStateT)
import Control.Monad.Trans.Either (eitherT)
import Data.EitherR (fmapL)
import Data.List (foldl')
import Data.Typeable (Typeable)
import LIO.Core (LIOState(LIOState), liftLIO, evalLIO, setClearanceP, taint, guardWrite)
import LIO.Label (lub)
import LIO.DCLabel (CNF, DCLabel, (%%), cFalse, toCNF)
import LIO.Error (AnyLabelError)
import LIO.TCB (Priv(PrivTCB))
import System.Log (Priority(DEBUG))

import LIO.Missing
import Thentos.Action.Types
import Thentos.Types

import qualified Thentos.Action.Unsafe as U
import qualified Thentos.Transaction as T


-- * running actions

ioExc :: Exception e => IO (Either e a, b) -> IO (a, b)
ioExc act = do
    (e, s) <- act
    either throwIO (return . flip (,) s) e

-- | Call 'runActionE' and throw 'Left' values.
runAction :: (Show e, Typeable e) => s -> ActionState -> Action e s a -> IO (a, s)
runAction polyState actionState action = do
    ioExc $ runActionE polyState actionState action

runActionWithPrivs :: (Show e, Typeable e) =>
    [CNF] -> s -> ActionState -> Action e s a -> IO (a, s)
runActionWithPrivs ars polyState actionState action = do
    ioExc $ runActionWithPrivsE ars polyState actionState action

runActionWithClearance :: (Show e, Typeable e) =>
    DCLabel -> s -> ActionState -> Action e s a -> IO (a, s)
runActionWithClearance label polyState actionState action = do
    ioExc $ runActionWithClearanceE label polyState actionState action

runActionAsAgent :: (Show e, Typeable e) =>
    Agent -> s -> ActionState -> Action e s a -> IO (a, s)
runActionAsAgent agent polyState actionState action = do
    ioExc $ runActionAsAgentE agent polyState actionState action

runActionInThentosSession :: (Show e, Typeable e) =>
    ThentosSessionToken -> s -> ActionState -> Action e s a -> IO (a, s)
runActionInThentosSession tok polyState actionState action = do
    ioExc $ runActionInThentosSessionE tok polyState actionState action

-- | Call an action with no access rights.  Catch all errors.  Initial LIO state is not
-- `dcDefaultState`, but @LIOState dcBottom dcBottom@: Only actions that require no clearance can be
-- executed, and the label has not been guarded by any action yet.
--
-- Updates to the polymorphic state inside the action are effective in the result if there are no
-- exceptions or if a `ThentosError` is thrown, but NOT if any other exceptions (such as
-- 'AnyLabelError') are thrown.
runActionE :: forall s e a. (Show e, Typeable e) =>
    s -> ActionState -> Action e s a -> IO (Either (ActionError e) a, s)
runActionE polyState actionState action = catchUnknown
  where
    inner :: Action e s a -> IO (Either (ThentosError e) a, s)
    inner = (`evalLIO` LIOState dcBottom dcBottom)
          . (`runStateT` polyState)
          . eitherT (return . Left) (return . Right)
          . (`runReaderT` actionState)
          . fromAction

    catchAnyLabelError = (first (fmapL ActionErrorThentos) <$> inner action)
        `catch` \e -> return (Left $ ActionErrorAnyLabel e, polyState)

    catchUnknown = catchAnyLabelError
        `catch` \e -> return (Left $ ActionErrorUnknown e, polyState)

runActionWithPrivsE :: (Show e, Typeable e) =>
    [CNF] -> s -> ActionState -> Action e s a -> IO (Either (ActionError e) a, s)
runActionWithPrivsE ars ps as =
    runActionE ps as . (grantAccessRights'P ars >>)

runActionWithClearanceE :: (Show e, Typeable e) =>
    DCLabel -> s -> ActionState -> Action e s a -> IO (Either (ActionError e) a, s)
runActionWithClearanceE label ps as =
    runActionE ps as . ((liftLIO $ setClearanceP (PrivTCB cFalse) label) >>)

runActionAsAgentE :: (Show e, Typeable e) =>
    Agent -> s -> ActionState -> Action e s a -> IO (Either (ActionError e) a, s)
runActionAsAgentE agent ps as =
    runActionE ps as . ((accessRightsByAgent'P agent >>= grantAccessRights'P) >>)

runActionInThentosSessionE :: (Show e, Typeable e) =>
    ThentosSessionToken -> s -> ActionState -> Action e s a -> IO (Either (ActionError e) a, s)
runActionInThentosSessionE tok ps as =
    runActionE ps as . ((accessRightsByThentosSession'P tok >>= grantAccessRights'P) >>)


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
grantAccessRights'P :: [CNF] -> Action e s ()
grantAccessRights'P ars = liftLIO $ setClearanceP (PrivTCB cFalse) c
  where
    c :: DCLabel
    c = foldl' lub dcBottom [ ar %% ar | ar <- ars ]

-- | Construct a 'DCLabel' from agent's roles.
accessRightsByAgent'P :: Agent -> Action e s [CNF]
accessRightsByAgent'P agent = makeAccessRights <$> U.unsafeAction (U.query $ T.agentRoles agent)
  where
    makeAccessRights :: [Role] -> [CNF]
    makeAccessRights roles = toCNF agent : map toCNF roles

accessRightsByThentosSession'P :: ThentosSessionToken -> Action e s [CNF]
accessRightsByThentosSession'P tok = do
    (_, session) <- U.unsafeAction . U.query . T.lookupThentosSession $ tok
    accessRightsByAgent'P $ session ^. thSessAgent


-- * better label errors

-- | Call 'taint', but log a more informative error in case of fail.
taintMsg :: String -> DCLabel -> Action e s ()
taintMsg msg l = do
    tryTaint l (return ()) $ \ (e :: AnyLabelError) -> do
        U.unsafeAction . U.logger DEBUG $ "taintMsg:\n    " ++ msg ++ "\n    " ++ show e
        liftLIO $ taint l

guardWriteMsg :: String -> DCLabel -> Action e s ()
guardWriteMsg msg l = do
    tryGuardWrite l (return ()) $ \ (e :: AnyLabelError) -> do
        U.unsafeAction . U.logger DEBUG $ "guardWrite:\n    " ++ msg ++ "\n    " ++ show e
        liftLIO $ guardWrite l
