{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE RankNTypes                  #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TypeFamilies                #-}

module Thentos.Action.Core
where

import Control.Monad.Reader (runReaderT)
import Control.Monad.State (runStateT)
import Control.Monad.Trans.Either (eitherT)

import LIO.Core (LIO)
import LIO.Missing
import Thentos.Action.Types
import Thentos.Action.TCB
import Thentos.Prelude
import Thentos.Types

import qualified Thentos.Action.Unsafe as U


-- * running actions

ioExc' :: Exception e => IO (Either e a) -> IO a
ioExc' act = either throwIO return =<< act

ioExc :: Exception e => IO (Either e a, b) -> IO (a, b)
ioExc act = do
    (e, s) <- act
    either throwIO (return . flip (,) s) e

-- | Call 'runActionE' and throw 'Left' values.
runAction :: (Show e, Typeable e) => s -> ActionState -> ActionStack e s a -> IO (a, s)
runAction polyState actionState action = do
    ioExc $ runActionE polyState actionState action

runActionWithPrivs :: (Show e, Typeable e) =>
    [CNF] -> s -> ActionState -> ActionStack e s a -> IO (a, s)
runActionWithPrivs ars polyState actionState action = do
    ioExc $ runActionWithPrivsE ars polyState actionState action

runActionWithClearance :: (Show e, Typeable e) =>
    DCLabel -> s -> ActionState -> ActionStack e s a -> IO (a, s)
runActionWithClearance label polyState actionState action = do
    ioExc $ runActionWithClearanceE label polyState actionState action

runActionAsAgent :: (Show e, Typeable e) =>
    Agent -> s -> ActionState -> ActionStack e s a -> IO (a, s)
runActionAsAgent agent polyState actionState action = do
    ioExc $ runActionAsAgentE agent polyState actionState action

runActionInThentosSession :: (Show e, Typeable e) =>
    ThentosSessionToken -> s -> ActionState -> ActionStack e s a -> IO (a, s)
runActionInThentosSession tok polyState actionState action = do
    ioExc $ runActionInThentosSessionE tok polyState actionState action

redirectLabelAndUnknownErrors :: MonadError (ActionError e) m => (m a -> IO b) -> IO b -> IO b
redirectLabelAndUnknownErrors f m =
    m `catch` (f . throwError . ActionErrorAnyLabel)
      `catch` (f . throwError . ActionErrorUnknown)

-- | Call an action with no access rights.  Catch all errors.  Initial LIO state is not
-- `dcDefaultState`, but @LIOState dcBottom dcBottom@: Only actions that require no clearance can be
-- executed, and the label has not been guarded by any action yet.
runLIOE :: MonadError (ActionError e) m => LIO DCLabel a -> IO (m a)
runLIOE = redirectLabelAndUnknownErrors return . fmap return . (`evalLIO` LIOState dcBottom dcBottom)

-- | Call an action with no access rights.  Catch all errors.  Initial LIO state is not
-- `dcDefaultState`, but @LIOState dcBottom dcBottom@: Only actions that require no clearance can be
-- executed, and the label has not been guarded by any action yet.
--
-- Updates to the polymorphic state inside the action are effective in the result if there are no
-- exceptions or if a `ThentosError` is thrown, but NOT if any other exceptions (such as
-- 'AnyLabelError') are thrown.
runActionE :: forall s e a. (Show e, Typeable e) =>
    s -> ActionState -> ActionStack e s a -> IO (Either (ActionError e) a, s)
runActionE polyState actionState = redirectLabelAndUnknownErrors (\x-> return (x, polyState)) . inner
  where
    inner :: ActionStack e s a -> IO (Either (ActionError e) a, s)
    inner = fmap (first (first ActionErrorThentos))
          . (`evalLIO` LIOState dcBottom dcBottom)
          . (`runStateT` polyState)
          . eitherT (return . Left) (return . Right)
          . (`runReaderT` actionState)
          . fromAction

runActionWithPrivsE :: (Show e, Typeable e) =>
    [CNF] -> s -> ActionState -> ActionStack e s a -> IO (Either (ActionError e) a, s)
runActionWithPrivsE ars ps as =
    runActionE ps as . (U.extendClearanceOnPrincipals ars >>)

runActionWithClearanceE :: (Show e, Typeable e) =>
    DCLabel -> s -> ActionState -> ActionStack e s a -> IO (Either (ActionError e) a, s)
runActionWithClearanceE label ps as =
    runActionE ps as . (U.extendClearanceOnLabel label >>)

runActionAsAgentE :: (Show e, Typeable e) =>
    Agent -> s -> ActionState -> ActionStack e s a -> IO (Either (ActionError e) a, s)
runActionAsAgentE agent ps as =
    runActionE ps as . (U.extendClearanceOnAgent agent >>)

runActionInThentosSessionE :: (Show e, Typeable e) =>
    ThentosSessionToken -> s -> ActionState -> ActionStack e s a -> IO (Either (ActionError e) a, s)
runActionInThentosSessionE tok ps as =
    runActionE ps as . (extendClearanceOnThentosSession tok >>)


-- * better label errors

-- | Call 'taint', but log a more informative error in case of fail.
taintMsg :: MonadThentosIO m => String -> DCLabel -> m ()
taintMsg msg l = do
    tryTaint l (return ()) $ \ (e :: AnyLabelError) -> do
        loggerA DEBUG $ "taintMsg:\n    " ++ msg ++ "\n    " ++ show e
        liftLIO $ taint l

guardWriteMsg :: MonadThentosIO m => String -> DCLabel -> m ()
guardWriteMsg msg l = do
    tryGuardWrite l (return ()) $ \ (e :: AnyLabelError) -> do
        loggerA DEBUG $ "guardWrite:\n    " ++ msg ++ "\n    " ++ show e
        liftLIO $ guardWrite l
