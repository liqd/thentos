{-# LANGUAGE DeriveGeneric                            #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}

module Thentos.TypesSpec where

import Data.Aeson (decode, FromJSON)
import Data.String.Conversions (cs)
import GHC.Generics (Generic)
import LIO (canFlowTo, lub, glb)
import LIO.DCLabel (DCLabel, (%%), (/\), (\/), toCNF)
import Test.Hspec.QuickCheck (modifyMaxSize)
import Test.Hspec (Spec, context, describe, it, shouldBe, hspec)
import Test.QuickCheck (property)

import Thentos.Types

import Thentos.Test.Arbitrary ()

testSizeFactor :: Int
testSizeFactor = 1

tests :: IO ()
tests = hspec spec

spec :: Spec
spec = modifyMaxSize (* testSizeFactor) $ do

    describe "ThentosLabel, ThentosClearance, DCLabel" $ do
      it "works (unittests)" $ do
        let a = toCNF ("a" :: String)
            b = toCNF ("b" :: String)
            c = toCNF ("c" :: String)

        and [ b \/ a %% a `canFlowTo` a %% a
            , a %% a /\ b `canFlowTo` a %% a
            , a %% (a /\ b) \/ (a /\ c) `canFlowTo` a %% a
            , not $ a %% (a /\ b) \/ (a /\ c) `canFlowTo` a %% b
            ,       True  %% False `canFlowTo` True %% False
            ,       True  %% False `canFlowTo` False %% True
            ,       True  %% True  `canFlowTo` False %% True
            ,       False %% False `canFlowTo` False %% True
            , not $ False %% True  `canFlowTo` True  %% False
            , not $ True  %% True  `canFlowTo` True  %% False
            , not $ False %% False `canFlowTo` True  %% False
            , let label = a \/ b %% a /\ b
                  clearance = a /\ c %% a \/ c
              in label `canFlowTo` clearance
            , let label = a \/ b %% a /\ b
                  label2 = a \/ c %% a /\ c
                  clearance = a /\ c %% a \/ c
              in lub label label2 `canFlowTo` clearance
            , True
            ] `shouldBe` True

      let (>>>) :: DCLabel -> DCLabel -> Bool = canFlowTo
          infix 5 >>>

          (<==>) = (==)
          infix 2 <==>

          -- (==>) a b = not a || b
          -- infix 2 ==>

      it "satisfies: l >>> l' && l' >>> l <==> l == l'" . property $
          \ l l' ->
              l >>> l' && l' >>> l <==> l == l'

      it "satisfies: l >>> l' <==> lub l l' == l'" . property $
          \ l l' ->
              l >>> l' <==> lub l l' == l'

      it "satisfies: l >>> l' <==> glb l l' == l" . property $
          \ l l' ->
              l >>> l' <==> glb l l' == l

      it "satisfies: l >>> c && l' >>> c <==> lub l l' >>> c" . property $
          \ l l' c ->
              l >>> c && l' >>> c <==> lub l l' >>> c

      it "satisfies: l >>> c && l >>> c' <==> l >>> glb c c'" . property $
          \ l c c' ->
              l >>> c && l >>> c' <==> l >>> glb c c'

    describe "ProxyUri" $ do

        context "its Show instance" $ do

            it "is a right inverse of decode" $ do
                let example = ProxyUri { proxyHost = "example.com"
                                       , proxyPort = 80
                                       , proxyPath = "/path"
                                       }
                decodeLenient (show example) `shouldBe` Just example

        context "its FromJSON instance" $ do

            it "allows simple http domains" $ do
                let correct = ProxyUri "something.com" 80 "/"
                decodeLenient "http://something.com/" `shouldBe` Just correct

            it "allows ports" $ do
                let correct = ProxyUri "something.com" 799 "/"
                decodeLenient "http://something.com:799/" `shouldBe` Just correct

            it "decodes paths" $ do
                let correct = ProxyUri "something.com" 799 "/path"
                decodeLenient "http://something.com:799/path" `shouldBe` Just correct

            it "only allows http" $ do
                decodeLenient "https://something.com" `shouldBe` Nothing
                decodeLenient "ftp://something.com" `shouldBe` Nothing

            it "does not allow query strings" $ do
                decodeLenient "http://something.com?hi" `shouldBe` Nothing

            it "does not allow fragments" $ do
                decodeLenient "http://something.com/t#hi" `shouldBe` Nothing

decodeLenient :: String -> Maybe ProxyUri
decodeLenient str = val <$> (decode . cs $ "{ \"val\" : \"" ++ str ++ "\"}")

-- @Wrapper@ is used to get around the restriction from top-level strings in
-- pre 0.9 versions of @aeson@
data Wrapper = Wrapper { val :: ProxyUri }
    deriving (Eq, Show, Generic)

instance FromJSON Wrapper
