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
{-# LANGUAGE TypeSynonymInstances                     #-}

module Thentos.Test.DefaultSpec
where

import Control.Monad.State (liftIO)
import Network.HTTP.Types.Status ()
import Network.Wai (Application)
import Network.Wai.Test (simpleBody)
import Test.Hspec (SpecWith, describe, it, shouldSatisfy)
import Test.Hspec.Wai (shouldRespondWith, get)

import qualified Data.ByteString.Lazy as LBS


specHasRestDocs :: SpecWith Application
specHasRestDocs = do
    describe "`RestDocs`" $ do
        let bodyNonEmpty resp = liftIO $ simpleBody resp `shouldSatisfy` ((>100) . LBS.length)

        it "has markdown" $ do
            get "/docs/md" `shouldRespondWith` 200
            get "/docs/md" >>= bodyNonEmpty
