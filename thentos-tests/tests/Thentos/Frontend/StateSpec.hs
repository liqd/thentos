{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Thentos.Frontend.StateSpec where

import Control.Lens ((^.), (%~))
import Control.Monad (when)
import Control.Monad.State (modify, gets, liftIO)
import Data.Maybe (catMaybes, fromMaybe)
import Data.String.Conversions (ST, LBS, SBS, cs)
import Network.HTTP.Types (RequestHeaders, methodGet, methodPost)
import Network.Wai (Application)
import Network.Wai.Test (SResponse, simpleBody, simpleHeaders, simpleStatus)
import Servant (Proxy(Proxy), ServerT, Capture, QueryParam, Post, Get, JSON, (:<|>)((:<|>)), (:>))
import Test.Hspec (Spec, hspec, before, around, describe, it,
                   shouldBe, shouldContain, shouldSatisfy, shouldNotSatisfy, pendingWith)
import Test.Hspec.Wai (request)
import Servant.Server.Internal.ServantErr (errHTTPCode, errHeaders, errBody)

import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.Attoparsec.Combinator as AP
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Types.Status as HT
import qualified Network.Wreq as Wreq

import Thentos.Action.Core
import Thentos.Config
import Thentos.Frontend.Handlers.Combinators (redirect')
import Thentos.Frontend.State
import Thentos.Frontend.Types
import Thentos.Types

import Thentos.Test.Config
import Thentos.Test.Core


tests :: IO ()
tests = hspec spec

spec :: Spec
spec = describe "Thentos.Frontend.State" . before testApp $ do
    spec_frontendState

spec_frontendState :: SpecWith Application
spec_frontendState = do

    it "is initialized with empty message queue" $ do
        resp <- request methodGet "" [] ""
        liftIO $ simpleBody resp `shouldBe` "[]"

    it "posts accumulate into message queue" $ do
        presp <- request methodPost "heya" [] ""
        gresp <- request methodGet "" (getCookie presp) ""
        liftIO $ simpleBody gresp `shouldBe`
            (cs . show . fmap show $ [FrontendMsgSuccess "heya"])

        presp2 <- request methodPost "ping" (getCookie gresp) ""
        gresp2 <- request methodGet "" (getCookie presp2) ""
        liftIO $ simpleBody gresp2 `shouldBe`
            (cs . show . fmap show $ [FrontendMsgSuccess "ping", FrontendMsgSuccess "heya"])


getCookie :: SResponse -> RequestHeaders
getCookie = fmap f . simpleHeaders
  where
    f ("Set-Cookie", c) = ("Cookie", c)
    f hdr = hdr


type TestApi =
       Capture "id" ST :> Post '[JSON] ()
  :<|> Get '[JSON] [ST]

testApi :: ServerT TestApi FAction
testApi = _post :<|> _read
  where
    _post msg = modify $ fsdMessages %~ (FrontendMsgSuccess msg :)
    _read = gets ((cs . show <$>) . (^. fsdMessages))

testApp :: IO Application
testApp = createActionState "thentos_test" thentosTestConfig
      >>= serveFAction (Proxy :: Proxy TestApi) testApi
