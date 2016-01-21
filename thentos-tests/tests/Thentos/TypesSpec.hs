{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Thentos.TypesSpec where

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson (decode, eitherDecode, encode, object, (.=), FromJSON, Value(String))
import Data.Pool (Pool)
import Data.String.Conversions (LBS, ST, cs)
import Database.PostgreSQL.Simple (Connection, Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import GHC.Generics (Generic)
import LIO (canFlowTo, lub, glb)
import LIO.DCLabel (DCLabel, (%%), (/\), (\/), toCNF)
import Test.Hspec.QuickCheck (modifyMaxSize)
import Test.Hspec (Spec, SpecWith, before, context, describe, it, shouldBe, pendingWith)
import Test.QuickCheck (property)

import Thentos.Types

import Thentos.Test.Arbitrary ()
import Thentos.Test.Config
import Thentos.Test.Core
import Thentos.Test.Transaction

testSizeFactor :: Int
testSizeFactor = 1

spec :: Spec
spec = do
    typesSpec
    before (thentosTestConfig >>= createDb) dbSpec

typesSpec :: Spec
typesSpec = modifyMaxSize (* testSizeFactor) $ do

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
                decodeProxy (show example) `shouldBe` Just example

        context "its FromJSON instance" $ do

            it "allows simple http domains" $ do
                let correct = ProxyUri "something.com" 80 "/"
                decodeProxy "http://something.com/" `shouldBe` Just correct

            it "allows ports" $ do
                let correct = ProxyUri "something.com" 799 "/"
                decodeProxy "http://something.com:799/" `shouldBe` Just correct

            it "decodes paths" $ do
                let correct = ProxyUri "something.com" 799 "/path"
                decodeProxy "http://something.com:799/path" `shouldBe` Just correct

            it "only allows http" $ do
                decodeProxy "https://something.com" `shouldBe` Nothing
                decodeProxy "ftp://something.com" `shouldBe` Nothing

            it "does not allow query strings" $ do
                decodeProxy "http://something.com?hi" `shouldBe` Nothing

            it "does not allow fragments" $ do
                decodeProxy "http://something.com/t#hi" `shouldBe` Nothing

    describe "secondsFromString" $ do

        it "parses and converts units correctly" $ do
            secondsFromString "1ms" `shouldBe` Just (0.001 :: Double)
            secondsFromString "1s"  `shouldBe` Just (1     :: Double)
            secondsFromString "1m"  `shouldBe` Just (60    :: Double)
            secondsFromString "1h"  `shouldBe` Just (3600  :: Double)
            secondsFromString "1d"  `shouldBe` Just (86400 :: Double)

        it "handles fractional values" $
            secondsFromString "0.5m" `shouldBe` Just (30 :: Double)

        it "handles negative values" $
            secondsFromString "-3m" `shouldBe` Just (-180 :: Double)

        it "requires a unit" $
            secondsFromString "10" `shouldBe` (Nothing :: Maybe Double)

        it "fails for empty input" $
            secondsFromString "" `shouldBe` (Nothing :: Maybe Double)


    describe "PasswordResetRequest" $ do
        it "has invertible *JSON instances" . property $
            \(p :: PasswordResetRequest) -> (eitherDecode . encode) p == Right p

        it "rejects short passwords" $ do
             pendingWith "FIXME length check not yet implemented"
             let reqdata = mkPwResetRequestJson "/principals/resets/dummypath" "short"
             (eitherDecode reqdata :: Either String PasswordResetRequest)
                 `shouldBe` Left "password too short (less than 6 characters)"


decodeProxy :: String -> Maybe ProxyUri
decodeProxy str = val <$> (decode . cs $ "{ \"val\" : \"" ++ str ++ "\"}")

mkPwResetRequestJson :: ST -> ST -> LBS
mkPwResetRequestJson path pass = encodePretty $ object
    ["path" .= String path, "password" .= String pass]

-- @Wrapper@ is used to get around the restriction from top-level strings in
-- pre 0.9 versions of @aeson@
data Wrapper = Wrapper { val :: ProxyUri }
    deriving (Eq, Show, Generic)

instance FromJSON Wrapper


dbSpec :: SpecWith (Pool Connection)
dbSpec = do
    describe "Timeout" $ do
        let testData =
              [ ("5 seconds",   fromSeconds 5)
              , ("20 minutes",  fromSeconds $ 20*60)
              , ("-1 hour",     fromSeconds $ -1*60*60)
              , ("0.1 seconds", fromMilliseconds 100)
              ] :: [(String, Timeout)]

        it "converts correctly from SQL intervals" $ \conns -> mapM_
            (\(i, t) -> do
                [Only res] <- doQuery conns [sql|SELECT interval ?|] (Only i)
                res `shouldBe` t)
            testData

        it "converts correctly to SQL intervals" $ \conns -> mapM_
            (\(i, t) -> do
                [Only res] <- doQuery conns [sql|SELECT interval ? = ?|] (i, t)
                res `shouldBe` True)
            testData

        it "converts correctly to SQL intervals in mixed expressions" $ \conns -> do
            [Only res1] <- doQuery conns [sql| SELECT now() < now() - ?::interval|] (Only $ fromMilliseconds 500)
            res1 `shouldBe` False
            [Only res2] <- doQuery conns [sql| SELECT now() < now() + ?::interval|] (Only $ fromMilliseconds 500)
            res2 `shouldBe` True
            [Only res3] <- doQuery conns [sql| SELECT '2015-02-03 12:01:02'::timestamp
                                                    = '2015-02-03 12:00:59'::timestamp
                                                    + ?::interval|] (Only $ fromSeconds 3)
            res3 `shouldBe` True
