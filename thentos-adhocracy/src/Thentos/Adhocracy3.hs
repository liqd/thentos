{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Thentos.Adhocracy3 (main) where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar (MVar, newMVar)
import Control.Concurrent (ThreadId, threadDelay, forkIO)
import Control.Exception (finally)
import Control.Monad (when, forever)
import Crypto.Random (ChaChaDRG, drgNew)
import Data.Acid (AcidState, openLocalStateFrom, createCheckpoint, closeAcidState)
import Data.Acid.Advanced (query', update')
import Data.Configifier ((>>.), Tagged(Tagged))
import Data.Either (isRight, isLeft)
import Data.Proxy (Proxy(Proxy))
import System.Log.Logger (Priority(DEBUG, INFO, ERROR), removeAllHandlers)
import Text.Show.Pretty (ppShow)

import System.Log.Missing (logger, announceAction)
import Thentos (makeMain)
import Thentos.Action
import Thentos.Action.Core (ActionState(..), runAction)
import Thentos.Adhocracy3.Types
import Thentos.Config
import Thentos.Util

import qualified Thentos.Adhocracy3.Backend.Api.Simple as Simple (runBackend)
import qualified Thentos.Adhocracy3.Backend.Api.Sso as Sso (runBackend)
import qualified Thentos.Transaction as T


-- * main

main :: IO ()
main = makeMain emptyDB $ \ (actionState@(ActionState (st, _, _))) mBeConfig mFeConfig cmd ->
    case cmd of
        ShowDB -> do
            logger INFO "database contents:"
            query' st T.SnapShot >>= either (error "oops?") (logger INFO . ppShow)

        Run -> do
            maybe (error "command `run` requires backend")
                (`Simple.runBackend` actionState)
                mBeConfig

        RunSso -> do
            maybe (error "command `runSso` requires backend")
                (`Sso.runBackend` actionState)
                mBeConfig
