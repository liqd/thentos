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
{-# LANGUAGE TypeOperators                            #-}
{-# LANGUAGE TypeSynonymInstances                     #-}

module Thentos.Backend.Api.PureScriptSpec (spec, tests)
where

import Control.Lens ((^.))
import Control.Monad.State (liftIO)
import Data.Configifier (Source(YamlString))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (cs, (<>))
import Data.String (fromString)
import Network.Wai (Application)
import Network.Wai.Test (simpleHeaders)
import Servant.API ((:>))
import Servant.Server (serve)
import System.FilePath ((</>))
import Test.Hspec (Spec, Spec, hspec, describe, context, it, shouldContain)
import Test.Hspec.Wai (shouldRespondWith, get)
import Test.Hspec.Wai.Internal (WaiSession, runWaiSession)

import Thentos.Action.Types
import Thentos.Test.Config
import Thentos.Test.Core

import qualified Thentos.Backend.Api.PureScript as PureScript


tests :: IO ()
tests = hspec spec

spec :: Spec
spec = describe "Thentos.Backend.Api.PureScript" specPurescript

specPurescript :: Spec
specPurescript = do
    let jsFile :: FilePath = "find-me.js"
        body   :: String   = "9VA4I5xpOAXRE"

    context "When purescript path not given in config" $ do
        it "response has status 404" . runSession False $ \tmp -> do
            liftIO $ writeFile (tmp </> jsFile) body
            get (cs $ "/js" </> jsFile) `shouldRespondWith` 404

    context "When path given in config" $ do
        it "response has right status, body, headers" . runSession True $ \tmp -> do
            liftIO $ writeFile (tmp </> jsFile) body
            get (cs $ "/js" </> jsFile) `shouldRespondWith` 200
            get (cs $ "/js" </> jsFile) `shouldRespondWith` fromString body
            resp <- get (cs $ "/js" </> jsFile)
            liftIO $ simpleHeaders resp `shouldContain` [("Content-Type", "application/javascript")]

runSession :: Bool -> (FilePath -> WaiSession a) -> IO a
runSession havePurescript session = outsideTempDirectory $ \tmp -> do
    app <- defaultApp havePurescript tmp
    runWaiSession (session tmp) app

defaultApp :: Bool -> FilePath -> IO Application
defaultApp havePurescript tmp = do
    cfg <- thentosTestConfig' [ YamlString $ "purescript: " <> cs tmp | havePurescript ]
    as <- createActionEnv' cfg
    return $! serve (Proxy :: Proxy Api) (PureScript.api (as ^. aStConfig))

type Api = "js" :> PureScript.Api
