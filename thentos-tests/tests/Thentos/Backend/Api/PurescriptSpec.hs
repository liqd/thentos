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
import Data.String.Conversions (cs)
import Network.Wai (Application)
import Network.Wai.Test (simpleBody)
import Test.Hspec (Spec, Spec, hspec, describe, context, it, shouldContain)
import Test.Hspec.Wai (shouldRespondWith, with, request)
import Servant.Server (serve, Server)
import Servant.API ((:<|>)((:<|>)))
import Data.Proxy (Proxy(Proxy))
import Data.Configifier ((>>.))

import Thentos.Action.Core

import qualified Thentos.Backend.Api.Purescript as Purescript
import qualified Thentos.Backend.Api.Simple as Simple

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
                request "GET" "/thentos.js" [] ""
                    `shouldRespondWith` 404

    -- The following assumes that you have installed thentos-purescript and pointed thentos there in
    -- the `purescript` field in config.
    context "When path given in config" . with (defaultApp True) $ do
        describe "*.js" $ do
            it "is available" $ do
                request "GET" "/thentos.js" [] ""
                    `shouldRespondWith` 200

            it "has the right content type" $ do
                resp <- request "GET" "/thentos.js" [] ""
                liftIO $ cs (simpleBody resp) `shouldContain` ("PS[\"Main\"].main();" :: String)


defaultApp :: Bool -> IO Application
defaultApp havePurescript = do
    as <- createActionState "test_thentos" thentosTestConfig
    return $! serve (Proxy :: Proxy Api) (api havePurescript as)

type Api = Simple.Api :<|> Purescript.Api

api :: Bool -> ActionState -> Server Api
api havePurescript  as@(ActionState (_, _, cfg)) = Simple.api as :<|> Purescript.api pursDir
  where
    pursDir :: Maybe FilePath
    pursDir = if havePurescript
        then cs <$> (cfg >>. (Proxy :: Proxy '["purescript"]))
        else Nothing
