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
import LIO.Core (MonadLIO, LIOState(LIOState), liftLIO, evalLIO, setLabel)
import LIO.DCLabel (DCLabel, (%%), (/\), (\/), dcPublic, ToCNF, toCNF)
import LIO.Error (AnyLabelError)
import LIO.TCB (LIO, ioTCB)

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

newtype ActionState = ActionState { fromActionState :: (AcidState DB, MVar SystemRNG, ThentosConfig) }
  deriving (Typeable, Generic)

newtype Action a = Action { fromAction :: ReaderT ActionState (EitherT ThentosError (LIO DCLabel)) a }
  deriving (Typeable, Generic)

instance Functor Action where
    fmap f (Action a) = Action $ fmap f a

instance Applicative Action where
    pure = Action . pure
    Action a <*> Action b = Action $ a <*> b

instance Monad Action where
    return = Action . return
    (Action a) >>= f = Action $ a >>= fromAction . f

instance MonadReader ActionState Action where
    ask = Action ask
    local f (Action a) = Action $ local f a

-- | FIXME: all exceptions that occur inside 'LIO' currently go uncaught until execution reaches
-- 'runAction' or 'runActionE'.  should errors caught there actually be handled here already, so we
-- can process them inside 'Action'?
instance MonadError ThentosError Action where
    throwError :: ThentosError -> Action a
    throwError = Action . throwError

    catchError :: Action a -> (ThentosError -> Action a) -> Action a
    catchError (Action a) handler = Action $ catchError a (fromAction . handler)

instance MonadLIO DCLabel Action where
    liftLIO lio = Action . ReaderT $ \ _ -> EitherT (Right <$> lio)


data ActionError =
    ActionErrorThentos ThentosError
  | ActionErrorAnyLabel AnyLabelError
  | ActionErrorUnknown SomeException
  deriving (Show, Typeable, Generic)

instance Exception ActionError


-- * running actions

runAction :: DCLabel -> ActionState -> Action a -> IO a
runAction clearance state action = runActionE clearance state action >>= either throwIO return

runActionE :: forall a . DCLabel -> ActionState -> Action a -> IO (Either ActionError a)
runActionE clearance state action = catchUnknown
  where
    inner :: IO (Either ThentosError a)
    inner = (`evalLIO` LIOState dcPublic clearance) .
            liftLIO .
            eitherT (return . Left) (return . Right) $
            fromAction action `runReaderT` state

    catchAnyLabelError :: IO (Either ActionError a)
    catchAnyLabelError = (fmapL ActionErrorThentos <$> inner) `catch` (return . Left . ActionErrorAnyLabel)

    catchUnknown :: IO (Either ActionError a)
    catchUnknown = catchAnyLabelError `catch` (return . Left . ActionErrorUnknown)


-- * labels and clearance

-- | Restrict confidentiality to list of principals or roles: After calling this action, at least
-- one of the elements of the argument list is required in the confidentiality part of the active
-- clearance level.
restrictRead :: ToCNF a => [a] -> Action ()
restrictRead xs = liftLIO . setLabel $ foldl' (/\) (toCNF True) xs %% True

-- | Restrict integrity to list of principals or roles: After calling this action, at least one of
-- the elements of the argument list is required in the integrity part of the active clearance
-- level.
restrictWrite :: ToCNF a => [a] -> Action ()
restrictWrite xs = liftLIO . setLabel $ True %% foldl' (\/) (toCNF True) xs

-- | Make transaction uncallable until somebody raises the label again.
restrictTotal :: Action ()
restrictTotal = liftLIO . setLabel $ False %% True

-- | Unravel role hierarchie stored under 'Agent' and construct a 'DCLabel'.  There is no guarantee
-- (at least not locally in this function) that the output will be finite.
clearanceByAgent :: Agent -> Action DCLabel
clearanceByAgent agent = makeClearance <$> query'P (AgentRoles agent)
  where
    makeClearance :: Set.Set Role -> DCLabel
    makeClearance roles = s %% i
      where
        s = Set.fold (/\) (toCNF agent) basicRoles
        i = Set.fold (\/) (toCNF agent) basicRoles

        basicRoles = Set.fold (flip f) Set.empty roles
          where
            f :: Set.Set RoleBasic -> Role -> Set.Set RoleBasic
            f acc (Roles rs) = foldl' f acc rs
            f acc (RoleBasic b) = Set.insert b acc

clearanceByThentosSession :: ThentosSessionToken -> Action DCLabel
clearanceByThentosSession = error "clearanceByThentosSession: not implemented"


-- * TCB business

-- | Call 'update'' on the 'ActionState' and re-throw the exception that has been turned into an
-- 'Either' on the border between acid-state and the real world.
update'P :: ( UpdateEvent event
            , EventState event ~ DB
            , EventResult event ~ Either ThentosError v
            ) => event -> Action v
update'P e = do
    ActionState (st, _, _) <- Action ask
    result <- liftLIO . ioTCB $ update' st e
    either throwError return result

-- | See 'updateA'.
query'P :: ( QueryEvent event
           , EventState event ~ DB
           , EventResult event ~ Either ThentosError v
           ) => event -> Action v
query'P e = do
    (ActionState (st, _, _)) <- Action ask
    result <- liftLIO . ioTCB $ query' st e
    either throwError return result

getConfig'P :: Action ThentosConfig
getConfig'P = (\ (ActionState (_, _, c)) -> c) <$> Action ask

getCurrentTime'P :: Action Timestamp
getCurrentTime'P = Timestamp <$> liftLIO (ioTCB Thyme.getCurrentTime)

-- | A relative of 'cprgGenerate' from crypto-random that lives in
-- 'Action'.
genRandomBytes'P :: Int -> Action SBS
genRandomBytes'P i = do
    let f :: SystemRNG -> (SystemRNG, SBS)
        f r = case cprgGenerate i r of (output, r') -> (r', output)
    ActionState (_, mr, _) <- Action ask
    liftLIO . ioTCB . modifyMVar mr $ return . f

makeUserFromFormData'P :: UserFormData -> Action User
makeUserFromFormData'P = liftLIO . ioTCB . makeUserFromFormData

hashUserPass'P :: UserPass -> Action (HashedSecret UserPass)
hashUserPass'P = liftLIO . ioTCB . hashUserPass

hashServiceKey'P :: ServiceKey -> Action (HashedSecret ServiceKey)
hashServiceKey'P = liftLIO . ioTCB . hashServiceKey

sendMail'P :: SmtpConfig -> Maybe UserName -> UserEmail -> ST -> ST -> Action ()
sendMail'P config mName address subject = liftLIO . ioTCB . sendMail config mName address subject

logger'P :: Priority -> String -> Action ()
logger'P prio = liftLIO . ioTCB . logger prio

-- | (This type signature could be greatly simplified, but that would also make it less explanatory.)
logIfError'P :: forall m l e v . (m ~ Action, Monad m, MonadLIO l m, MonadError e m, Show e) => m v -> m v
logIfError'P = (`catchError` f)
  where
    f e = do
        logger'P DEBUG $ "*** error: " ++ show e
        throwError e
