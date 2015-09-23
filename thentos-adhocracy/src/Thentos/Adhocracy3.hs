module Thentos.Adhocracy3 (main) where

import Thentos (makeMain, createConnPoolAndInitDb)
import Thentos.Config

import qualified Thentos.Adhocracy3.Backend.Api.Simple as Simple (runBackend)
import qualified Thentos.Adhocracy3.Backend.Api.Sso as Sso (runBackend)


-- * main

main :: IO ()
main = makeMain $ \actionState mBeConfig _ cmd ->
    case cmd of
        Run -> do
            maybe (error "command `run` requires backend")
                (`Simple.runBackend` actionState)
                mBeConfig

        RunSso -> do
            maybe (error "command `runSso` requires backend")
                (`Sso.runBackend` actionState)
                mBeConfig
