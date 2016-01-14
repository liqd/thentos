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
import Data.String.Conversions (cs)
import Data.String (fromString)
import Network.Wai (Application)
import Network.Wai.Test (simpleHeaders)
import Servant.API ((:>))
import Servant.Server (serve, Server)
import System.FilePath ((</>))
import Test.Hspec (Spec, Spec, hspec, describe, context, around, around_, it, shouldContain)
import Test.Hspec.Wai (shouldRespondWith, with, get)
import Test.Hspec.Wai.Internal (WaiSession, runWaiSession)

import Thentos.Config (getConfigWithSources)
import Thentos.Action.Types
import Thentos.Test.Config
import Thentos.Test.Core

import qualified Thentos.Backend.Api.PureScript as PureScript


tests :: IO ()
tests = hspec spec

spec :: Spec
spec = describe "Thentos.Backend.Api.PureScript" specPurescript

specPurescript :: Spec
specPurescript = around outsideTempDirectory $ do
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

runSession :: Bool -> (FilePath -> WaiSession a) -> FilePath -> IO a
runSession havePurescript session tmp = defaultApp havePurescript >>= runWaiSession (session tmp)

defaultApp :: Bool -> IO Application
defaultApp havePurescript = do
    srcs <- thentosTestConfigSources
    cfg <- getConfigWithSources $ srcs ++ [ YamlString "purescript: ." | havePurescript ]
    as <- createActionState' cfg
    return $! serve (Proxy :: Proxy Api) (api havePurescript as)

type Api = "js" :> PureScript.Api

api :: Bool -> ActionState -> Server Api
api True as = PureScript.api (as ^. aStConfig)
api False _ = PureScript.api' Nothing
