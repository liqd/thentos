{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE PackageImports              #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TypeFamilies                #-}

module Thentos.Action.Core
where

import Control.Arrow (first)
import Control.Concurrent (modifyMVar)
import Control.Exception (Exception, throwIO, catch, ErrorCall(..))
import Control.Lens ((^.))
import Control.Monad.Except (MonadError, throwError, catchError)
import Control.Monad.Reader (runReaderT, ask)
import Control.Monad.State (runStateT)
import Control.Monad.Trans.Either (eitherT)
import "cryptonite" Crypto.Random (ChaChaDRG, randomBytesGenerate)
import Data.Pool (withResource)
import Data.EitherR (fmapL)
import Data.List (foldl')
import Data.String.Conversions (LT, ST, SBS)
import Data.Typeable (Typeable)
import LIO.Core (MonadLIO, LIOState(LIOState), liftLIO, evalLIO, setClearanceP, taint, guardWrite)
import LIO.Label (lub)
import LIO.DCLabel (CNF, DCLabel, (%%), cFalse, toCNF)
import LIO.Error (AnyLabelError)
import LIO.TCB (Priv(PrivTCB), ioTCB)
import System.Log (Priority(DEBUG, CRITICAL))
import Text.Hastache (MuConfig(..), MuContext, defaultConfig, emptyEscape, hastacheStr)

import qualified Data.Thyme as Thyme

import LIO.Missing
import System.Log.Missing (logger)
import Thentos.Action.Types
import Thentos.Config
import Thentos.Smtp
import Thentos.Transaction.Core (ThentosQuery, runThentosQuery)
import Thentos.Types
import Thentos.Util

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
accessRightsByAgent'P agent = makeAccessRights <$> query'P (T.agentRoles agent)
  where
    makeAccessRights :: [Role] -> [CNF]
    makeAccessRights roles = toCNF agent : map toCNF roles

accessRightsByThentosSession'P :: ThentosSessionToken -> Action e s [CNF]
accessRightsByThentosSession'P tok = do
    (_, session) <- query'P (T.lookupThentosSession tok)
    accessRightsByAgent'P $ session ^. thSessAgent


-- * TCB business

query'P :: ThentosQuery e v -> Action e s v
query'P u = do
    ActionState (connPool, _, _) <- Action ask
    result <- liftLIO . ioTCB . withResource connPool $ \conn -> runThentosQuery conn u
    either throwError return result

getConfig'P :: Action e s ThentosConfig
getConfig'P = (\ (ActionState (_, _, c)) -> c) <$> Action ask

getCurrentTime'P :: Action e s Timestamp
getCurrentTime'P = Timestamp <$> liftLIO (ioTCB Thyme.getCurrentTime)

-- | A relative of 'cprgGenerate' from crypto-random that lives in
-- 'Action'.
genRandomBytes'P :: Int -> Action e s SBS
genRandomBytes'P i = do
    let f :: ChaChaDRG -> (ChaChaDRG, SBS)
        f r = case randomBytesGenerate i r of (output, r') -> (r', output)
    ActionState (_, mr, _) <- Action ask
    liftLIO . ioTCB . modifyMVar mr $ return . f

makeUserFromFormData'P :: UserFormData -> Action e s User
makeUserFromFormData'P = liftLIO . ioTCB . makeUserFromFormData

hashUserPass'P :: UserPass -> Action e s (HashedSecret UserPass)
hashUserPass'P = liftLIO . ioTCB . hashUserPass

hashServiceKey'P :: ServiceKey -> Action e s (HashedSecret ServiceKey)
hashServiceKey'P = liftLIO . ioTCB . hashServiceKey

sendMail'P :: SmtpConfig -> Maybe UserName -> UserEmail -> ST -> ST -> Action e s ()
sendMail'P config mName address subject msg = liftLIO . ioTCB $ do
    result <- sendMail config mName address subject msg
    case result of
        Right () -> return ()
        Left (SendmailError s) -> do
            logger CRITICAL $ "error sending mail: " ++ s
            throwIO $ ErrorCall "error sending email"

-- | Render a Hastache template for plain-text output (none of the characters in context variables
-- will be escaped).
renderTextTemplate'P :: ST -> MuContext IO -> Action e s LT
renderTextTemplate'P template context =
    liftLIO . ioTCB $ hastacheStr hastacheCfg template context
  where
    hastacheCfg = defaultConfig { muEscapeFunc = emptyEscape }

logger'P :: Priority -> String -> Action e s ()
logger'P prio = liftLIO . ioTCB . logger prio

-- | (This type signature could be greatly simplified, but that would also make it less explanatory.)
logIfError'P :: forall m l e v f s
    . (m ~ Action f s, Monad m, MonadLIO l m, MonadError e m, Show e, Show f)
    => m v -> m v
logIfError'P = (`catchError` f)
  where
    f e = do
        logger'P DEBUG $ "*** error: " ++ show e
        throwError e


-- * better label errors

-- | Call 'taint', but log a more informative error in case of fail.
taintMsg :: String -> DCLabel -> Action e s ()
taintMsg msg l = do
    tryTaint l (return ()) $ \ (e :: AnyLabelError) -> do
        logger'P DEBUG $ ("taintMsg:\n    " ++ msg ++ "\n    " ++ show e)
        liftLIO $ taint l

guardWriteMsg :: String -> DCLabel -> Action e s ()
guardWriteMsg msg l = do
    tryGuardWrite l (return ()) $ \ (e :: AnyLabelError) -> do
        logger'P DEBUG $ "guardWrite:\n    " ++ msg ++ "\n    " ++ show e
        liftLIO $ guardWrite l
