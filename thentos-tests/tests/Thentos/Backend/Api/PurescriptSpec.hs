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

module Thentos.Backend.Api.PurescriptSpec (spec, tests)
where

import Control.Monad.State (liftIO)
import Data.CaseInsensitive (mk)
import Data.Configifier ((>>.))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (cs)
import Data.String (fromString)
import Network.Wai (Application)
import Network.Wai.Test (simpleBody, simpleHeaders)
import Servant.API ((:>))
import Servant.Server (serve, Server)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Hspec (Spec, Spec, hspec, describe, context, around_, it, shouldContain)
import Test.Hspec.Wai (shouldRespondWith, with, get)

import Thentos.Action.Core

import qualified Thentos.Backend.Api.Purescript as Purescript

import Thentos.Test.Config
import Thentos.Test.Core


tests :: IO ()
tests = hspec spec

spec :: Spec
spec = describe "Thentos.Backend.Api.Purescript" specPurescript

specPurescript :: Spec
specPurescript = do
    context "When purescript path not given in config" . with (defaultApp False) $ do
        describe "*.js" $ do
            it "is not available" $ do
                get "/js/thentos.js"
                    `shouldRespondWith` 404

    -- The following assumes that you have installed thentos-purescript and pointed thentos there in
    -- the `purescript` field in config.
    context "When path given in config" . with (defaultApp True) $ do
        describe "/js/*.js" $ do
            it "is available" $ do
                get "/js/thentos.js"
                    `shouldRespondWith` 200

            it "has the right content type" $ do
                resp <- get "/js/thentos.js"
                liftIO $ simpleHeaders resp
                    `shouldContain` [(mk "Content-Type", "application/javascript")]

            it "contains a purescript main function" $ do
                resp <- get "/js/thentos.js"
                liftIO $ cs (simpleBody resp) `shouldContain` ("PS[\"Main\"].main();" :: String)

    context "When reading purescript file system location from config"
        . around_ withLogger
        . with (defaultApp True) $ do
        it "honours the config" $ do
            let body :: String = "9VA4I5xpOAXRE"
                file :: FilePath = "honour-it.js"
                Just (path :: FilePath) = cs <$> (thentosTestConfig >>. (Proxy :: Proxy '["purescript"]))
            liftIO $ do
                createDirectoryIfMissing True path
                writeFile (path </> file) body
            get (cs $ "/js" </> file) `shouldRespondWith` fromString body

defaultApp :: Bool -> IO Application
defaultApp havePurescript = do
    as <- createActionState "test_thentos" thentosTestConfig
    return $! serve (Proxy :: Proxy Api) (api havePurescript as)

type Api = "js" :> Purescript.Api

api :: Bool -> ActionState -> Server Api
api True (ActionState (_, _, cfg)) = Purescript.api cfg
api False _ = Purescript.api' Nothing
