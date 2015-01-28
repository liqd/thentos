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
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT, left)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Data.Acid (AcidState, QueryEvent, UpdateEvent, EventState, EventResult)
import Data.Acid.Advanced (update', query')
import Data.Thyme.Time ()
import Servant.API ((:<|>)((:<|>)))

import DB
import Types


type RoutingState = (AcidState DB, DB -> Either DbError ThentosClearance)
type RestActionRaw = EitherT (Int, String) IO
type RestActionLabeled = ReaderT RoutingState RestActionRaw

updateServant :: forall event a .
                 ( UpdateEvent event
                 , EventState event ~ DB
                 , EventResult event ~ Either DbError a
                 ) => (ThentosClearance -> event) -> RestActionLabeled a
updateServant = accessServant update'

queryServant :: forall event a .
                 ( QueryEvent event
                 , EventState event ~ DB
                 , EventResult event ~ Either DbError a
                 ) => (ThentosClearance -> event) -> RestActionLabeled a
queryServant = accessServant query'

-- | Pull a snapshot from the database, pass it to the clearance
-- function in the reader monad, pass the resulting clearance to the
-- uncleared event, and pass the cleared event to either query' or
-- update' (whichever is passed as first arg).
--
-- NOTE: Authentication check and transaction do *not* form an atomic
-- transaction.  In order to get an upper bound on how long changes in
-- access priviledges need to become effective, the following may work
-- (but is not implemented): 'SnapShot' returns 'DB' together with a
-- timestamp.  When the actual transaction is executed, mkAuth will be
-- passed another timestamp (of the time of the execution of the
-- actual transaction), and can compare the two.  If the 'DB' snapshot
-- is too old, it can throw an error.  (This would mean that mkAuth
-- would have to live in 'RestActionLabeled', but I don't see any
-- issues with that.)  Anyway this is a little over-complicated for
-- now, I think.
accessServant :: forall event a .
                 ( EventState event ~ DB
                 , EventResult event ~ Either DbError a
                 ) => (AcidState (EventState event) -> event -> RestActionLabeled (EventResult event))
                   -> (ThentosClearance -> event) -> RestActionLabeled a
accessServant access unclearedEvent = do
    (st, clearanceAbs) <- ask
    clearanceE :: Either DbError ThentosClearance <- (>>= clearanceAbs) <$> query' st (SnapShot allowEverything)
    case clearanceE of
        Left err -> lift . left . showDbError $ err
        Right clearance -> do
            result <- access st (unclearedEvent clearance)
            case result of
                Left err -> lift . left . showDbError $ err
                Right success -> return success


-- | this is a work-around: The Server type family always terminates
-- in 'RestActionRaw' on all methods.  'PushReaderT' instances
-- transform handlers implemented in a monad stack that contains the
-- acid state and authentication information in a reader into the
-- handlers that we need.
class PushReaderT a where
    type PushReaderSubType a
    unPushReaderT :: RoutingState -> PushReaderSubType a -> a

instance (PushReaderT b) => PushReaderT (a -> b) where
    type PushReaderSubType (a -> b) = a -> PushReaderSubType b
    unPushReaderT clearance f = unPushReaderT clearance . f

instance (PushReaderT a, PushReaderT b) => PushReaderT (a :<|> b) where
    type PushReaderSubType (a :<|> b) = PushReaderSubType a :<|> PushReaderSubType b
    unPushReaderT clearance (a :<|> b) = unPushReaderT clearance a :<|> unPushReaderT clearance b

instance PushReaderT (RestActionRaw a) where
    type PushReaderSubType (RestActionRaw a) = RestActionLabeled a
    unPushReaderT rstate x = runReaderT x rstate
