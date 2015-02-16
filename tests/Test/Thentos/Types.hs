{-# LANGUAGE ScopedTypeVariables                      #-}

module Test.Thentos.Types where

import Data.SafeCopy (safeGet, safePut)
import Data.Serialize.Get (runGet)
import Data.Serialize.Put (runPut)
import LIO (canFlowTo, lub, glb)
import LIO.DCLabel ((%%), (/\), (\/), toCNF)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property)

import Thentos.Types

import Test.Arbitrary ()


tests :: Spec
tests = do
    describe "Thentos.Types" $ do
        describe "instance SafeCopy (HashedSecret a)" $
            it "is invertible" $ property $
                \ (pw :: HashedSecret a) ->
                    runGet safeGet (runPut $ safePut pw) == Right pw

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

      let (>>>) = canFlowTo
          infix 5 >>>

          (<==>) = (==)
          infix 2 <==>

          -- (==>) a b = not a || b
          -- infix 2 ==>

      it "satisfies: l >>> l' && l' >>> l <==> (l == l')" . property $
          \ (ThentosLabel l) (ThentosLabel l') ->
              l >>> l' && l' >>> l <==> l == l'

      it "satisfies: l >>> l' <==> (lub l l' == l')" . property $
          \ (ThentosLabel l) (ThentosLabel l') ->
              l >>> l' <==> lub l l' == l'

      it "satisfies: l >>> l' <==> (glb l l' == l)" . property $
          \ (ThentosLabel l) (ThentosLabel l') ->
              l >>> l' <==> glb l l' == l

      it "satisfies: l >>> c && l' >>> c <==> lub l l' >>> c" . property $
          \ (ThentosLabel l) (ThentosLabel l') (ThentosClearance c) ->
              l >>> c && l' >>> c <==> lub l l' >>> c

      it "satisfies: l >>> c && l >>> c' <==> l >>> glb c c'" . property $
          \ (ThentosLabel l) (ThentosClearance c) (ThentosClearance c') ->
              l >>> c && l >>> c' <==> l >>> glb c c'
