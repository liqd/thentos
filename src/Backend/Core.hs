{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE ExistentialQuantification                #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE InstanceSigs                             #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TupleSections                            #-}
{-# LANGUAGE TypeFamilies                             #-}
{-# LANGUAGE TypeOperators                            #-}
{-# LANGUAGE TypeSynonymInstances                     #-}
{-# LANGUAGE UndecidableInstances                     #-}

{-# OPTIONS  #-}

module Backend.Core
where

import Control.Applicative ((<$>))
import Control.Monad.Trans.Either (EitherT(EitherT), runEitherT)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Thyme.Time ()
import Servant.API ((:<|>)((:<|>)))

import Control.Concurrent.MVar (MVar)
import Crypto.Random (SystemRNG)

import Api
import DB


type RestAction      = Action (MVar SystemRNG)
type RestActionState = ActionState (MVar SystemRNG)
type RestActionRaw   = EitherT RestError IO
type RestError       = (Int, String)


-- | This is a work-around: The 'Server' type family terminates in
-- 'RestActionRaw' on all methods.  'PushReaderT' instances transform
-- handlers implemented in a monad stack we want (providing acid
-- state, clearance info, random generator, ... in a reader) into the
-- handlers in 'RestActionRaw'.  (Also, translate 'DbError' to
-- 'RestError'.)
class PushReaderT a where
    type PushReaderSubType a
    unPushReaderT :: RestActionState -> PushReaderSubType a -> a

instance (PushReaderT b) => PushReaderT (a -> b) where
    type PushReaderSubType (a -> b) = a -> PushReaderSubType b
    unPushReaderT clearance f = unPushReaderT clearance . f

instance (PushReaderT a, PushReaderT b) => PushReaderT (a :<|> b) where
    type PushReaderSubType (a :<|> b) = PushReaderSubType a :<|> PushReaderSubType b
    unPushReaderT clearance (a :<|> b) = unPushReaderT clearance a :<|> unPushReaderT clearance b

instance PushReaderT (RestActionRaw a) where
    type PushReaderSubType (RestActionRaw a) = RestAction a
    unPushReaderT restState restAction = fmapLTM showDbError $ runReaderT restAction restState


-- | Like 'fmapLT' from "Data.EitherR", but with the update of the
-- left value constructed in an impure action.
fmapLTM :: (Monad m, Functor m) => (a -> m a') -> EitherT a m b -> EitherT a' m b
fmapLTM trans e = EitherT $ do
    result <- runEitherT e
    case result of
        Right r -> return $ Right r
        Left l -> Left <$> trans l
