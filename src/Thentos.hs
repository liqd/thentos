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

{-# OPTIONS  #-}

module Thentos (main) where

import Control.Applicative ((<$>))
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.MVar (MVar, newMVar)
import Control.Exception (bracket_, finally)
import Control.Monad (void)
import Crypto.Random (SystemRNG, createEntropyPool, cprgCreate)
import Data.Acid (AcidState, openLocalStateFrom, createCheckpoint, closeAcidState)
import Data.Acid.Advanced (query')
import Data.Configifier ((>>.), Tagged(Tagged))
import Data.Proxy (Proxy(Proxy))
import System.Log.Logger (Priority(INFO), removeAllHandlers)
import Text.Show.Pretty (ppShow)

import Thentos.Config
import Thentos.Types
import Thentos.DB
import Thentos.Frontend (runFrontend)
import System.Log.Missing (logger)

import qualified Thentos.Backend.Api.Simple (runBackend)
import qualified Thentos.Backend.Api.Adhocracy3 (runBackend)


-- * main

main :: IO ()
main =
  do
    let notify :: String -> IO a -> IO a
        notify msg action = bracket_ (logger INFO msg)
                                     (logger INFO $ msg ++ ": [ok]") action

    st :: AcidState DB <- notify "setting up acid-state" $ openLocalStateFrom ".acid-state/" emptyDB
        -- (opening acid-state can take rather long if a large
        -- changelog needs to be replayed.  use asci-progress here?
        -- even though that would probably require patching
        -- acid-state.)

    rng :: MVar SystemRNG <- createEntropyPool >>= newMVar . cprgCreate
    config :: ThentosConfig <- getConfig "devel.config"
    configLogger
    _ <- createCheckpointLoop st 16000 Nothing
    createDefaultUser st (Tagged <$> config >>. (Proxy :: Proxy '["default_user"]))

    let mBeConfig :: Maybe HttpConfig
        mBeConfig = Tagged <$> config >>. (Proxy :: Proxy '["backend"])

        mFeConfig :: Maybe HttpConfig
        mFeConfig = Tagged <$> config >>. (Proxy :: Proxy '["frontend"])

    logger INFO "Press ^C to abort."
    let run = case config >>. (Proxy :: Proxy '["command"]) of
            ShowDB -> do
                logger INFO "database contents:"
                query' st (SnapShot allowEverything) >>= either (error "oops?") (logger INFO . ppShow)

            Run -> do
                let backend = maybe (return ())
                        (`Thentos.Backend.Api.Simple.runBackend` (st, rng, config))
                        mBeConfig
                let frontend = maybe (return ())
                        (`runFrontend` (st, rng, config))
                        mFeConfig

                void $ concurrently backend frontend

            RunA3 -> do
                maybe (error "command `runa3` requires `--runbackend`")
                    (`Thentos.Backend.Api.Adhocracy3.runBackend` (st, rng, config))
                    mBeConfig

    let finalize = do
            notify "creating checkpoint and shutting down acid-state" $
                createCheckpoint st >> closeAcidState st
            notify "shutting down hslogger" $
                removeAllHandlers

    run `finally` finalize
