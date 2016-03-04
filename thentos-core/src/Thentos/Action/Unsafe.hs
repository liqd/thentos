{-# LANGUAGE Unsafe                      #-}

{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE ScopedTypeVariables         #-}

module Thentos.Action.Unsafe
where

import LIO.Core (setClearanceP)
import LIO.TCB (Priv(PrivTCB), ioTCB)

import Thentos.Action.Types
import Thentos.Prelude
import Thentos.Transaction.Core (ThentosQuery, runThentosQuery)
import Thentos.Types

import qualified Thentos.Transaction as T


-- * making unsafe actions safe

unsafeLiftIO :: MonadThentosIO m => IO a -> m a
unsafeLiftIO = liftLIO . ioTCB

-- * queries

query :: MonadQuery e m => ThentosQuery e v -> m v
query u = do
    ActionEnv _ _ connPool <- ask
    unsafeLiftIO (runThentosQuery connPool u) >>= either throwError return


-- * labels, privileges and access rights.

extendClearanceOnLabel :: MonadThentosIO m => DCLabel -> m ()
extendClearanceOnLabel label = liftLIO $ do
    getClearance >>= setClearanceP (PrivTCB cFalse) . (`lub` label)

extendClearanceOnPrincipals :: MonadThentosIO m => ToCNF cnf => [cnf] -> m ()
extendClearanceOnPrincipals principals = mapM_ extendClearanceOnLabel $ [ p %% p | p <- principals ]

extendClearanceOnAgent :: MonadQuery e m => Agent -> m ()
extendClearanceOnAgent agent = do
    extendClearanceOnPrincipals [agent]
    query (T.agentGroups agent) >>= extendClearanceOnPrincipals
