{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

{-# OPTIONS_GHC  #-}

module Thentos.UtilSpec where

import Data.String.Conversions (ST, SBS)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import qualified Data.Binary

import Thentos.Types
import Thentos.Util

import Thentos.Test.Arbitrary ()
import Thentos.Test.Config (forceUserEmail)


mkUser :: HashedSecret UserPass -> User
mkUser h = User "" h (forceUserEmail "a@b.c")

mkService :: HashedSecret ServiceKey -> Service
mkService h = Service h (UserId 0) Nothing "name" "description"

spec :: Spec
spec = describe "Thentos.Util" $ do
    describe "HashedSecret" $ do
        it "has working binary instance." $ do
            let f h = (Data.Binary.decode . Data.Binary.encode) h `shouldBe` h
            mapM_ f [ BCryptHash ""
                    , BCryptHash "..."
                    , SCryptHash ""
                    , SCryptHash "..."
                    ]

    describe "UserPass <-> HashedSecret" $ do
        let f p = do
                h <- hashUserPass p
                verifyUserPass p (mkUser h) `shouldBe` True
        it "works." $ do
            f ""
            f "..."
            f "esZ2t/Wos4pNU"

    describe "ServiceKey <-> HashedSecret" $ do
        let f k = do
                h <- hashServiceKey k
                verifyServiceKey k (mkService h) `shouldBe` True
        it "works." $ do
            f ""
            f "..."
            f "esZ2t/Wos4pNU"

    -- The samples for this test have been generated with the following python script:
    --
    -- >>> from cryptacular.bcrypt import BCRYPTPasswordManager
    -- >>> manager = BCRYPTPasswordManager()
    -- >>>
    -- >>> def run(password):
    -- >>>     print "(" + repr(password) + ", " + repr(manager.encode(password)) + ")"
    -- >>>
    -- >>> run('')
    -- >>> run('***')
    -- >>> run('Cz3FWh613Dq.I')
    -- >>> run('aI0ZUDmx0DVJI')
    -- >>> run('jaDEzQ7MQpN26')
    -- >>> run('„¡33 € – hilfäh!“')
    --
    -- Note that the unicode string literal in the last line looks a little different in Haskell.
    describe "bcrypt verification" $ do
        let run (clear :: ST) (hashed :: SBS) = (clear, verdict) `shouldSatisfy` snd
                where verdict :: Bool = verifyUserPass (UserPass clear) (mkUser (BCryptHash hashed))

            samples = [ ("", "$2a$10$5lEQtZWJ9BglditOGuARrugb8g79hXeMhc7aWtNY5/QowmxEcSnBi")
                      , ("***", "$2a$10$Ktrbw39lib1doqd.hSQ7UOKSkuLYIsUbTrcEsYPofsnrkIsGFCaXW")
                      , ("Cz3FWh613Dq.I", "$2a$10$P9XIRAt3BRuJMlWErMJGZOqFqaw57o/SmfmwIW0CI9.Mv.w8EIkLe")
                      , ("aI0ZUDmx0DVJI", "$2a$10$xohDX.tn1yVoc4Bl5djLQ.L3nMc02mVVj0DNAc88faLNhlKYDB1DC")
                      , ("jaDEzQ7MQpN26", "$2a$10$ynKapqrChDtvvUuvSi5/teD3oeRW.QMpSawe8TR3qZ9JqDoh2qpii")
                      , ("„¡33 € – hilfäh!“", "$2a$10$MqxOGleJdX2KszMciuTNVOYWlMv1ae7WzUXHw8iLSAIhd19AFIPgy")
                      ]

        it "works." $ mapM_ (uncurry run) samples

    describe "bad password" $ do
        it "falsifies." $ do
            s <- mkService <$> hashServiceKey "good"
            verifyServiceKey "bad" s `shouldBe` False

            u <- mkUser <$> hashUserPass "good"
            verifyUserPass "bad" u `shouldBe` False

            let u' = mkUser (BCryptHash "$2a$10$5lEQtZWJ9BglditOGuARrugb8g79hXeMhc7aWtNY5/QowmxEcSnBi")
            verifyUserPass "bad" u' `shouldBe` False
