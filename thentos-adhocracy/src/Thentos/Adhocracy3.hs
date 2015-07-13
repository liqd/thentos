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

module Thentos.Adhocracy3 (main) where

import Data.Acid.Advanced (query')
import System.Log.Logger (Priority(INFO))
import Text.Show.Pretty (ppShow)

import System.Log.Missing (logger)
import Thentos (makeMain)
import Thentos.Action.Core (ActionState(..))
import Thentos.Adhocracy3.Types
import Thentos.Config

import qualified Thentos.Adhocracy3.Backend.Api.Simple as Simple (runBackend)
import qualified Thentos.Adhocracy3.Backend.Api.Sso as Sso (runBackend)
import qualified Thentos.Transaction as T


-- * main

main :: IO ()
main = makeMain emptyDB $ \ (actionState@(ActionState (st, _, _))) mBeConfig _ cmd ->
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
