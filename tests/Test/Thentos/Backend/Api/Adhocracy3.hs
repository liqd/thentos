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
{-# LANGUAGE ViewPatterns                             #-}

{-# OPTIONS  #-}

module Test.Thentos.Backend.Api.Adhocracy3
where

import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (property)

import qualified Data.Aeson as Aeson

import Thentos.Types
import Thentos.Backend.Api.Adhocracy3

import Test.Arbitrary ()


tests :: Spec
tests = do
    describe "Thentos.Backend.Api.Adhocracy3" $ do
        describe "A3UserNoPass" $ do
            it "has invertible *JSON instances" . property $
                let (===) (Right (A3UserNoPass (UserFormData n "" e)))
                          (Right (A3UserNoPass (UserFormData n' _ e')))
                              = n == n' && e == e'
                    (===) _ _ = False
                in \ (A3UserNoPass -> u) -> (Aeson.eitherDecode . Aeson.encode) u === Right u
        describe "A3UserWithPassword" $ do
            it "has invertible *JSON instances" . property $
                let (===) (Right (A3UserWithPass (UserFormData n "[password hidden]" e)))
                          (Right (A3UserWithPass (UserFormData n' _ e')))
                              = n == n' && e == e'
                    (===) _ _ = False
                in \ (A3UserWithPass -> u) -> (Aeson.eitherDecode . Aeson.encode) u === Right u
