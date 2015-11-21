{-# LANGUAGE DeriveGeneric                            #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE QuasiQuotes                              #-}
{-# LANGUAGE ScopedTypeVariables                      #-}

module Thentos.Sybil.CaptchaSpec where

import Control.Concurrent (forkIO)
import Data.String.Conversions (cs)
import System.IO (hFlush, hClose)
import System.Process (runInteractiveCommand, system)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)
import Data.Word8
import Control.Monad.Random

import qualified Data.ByteString as SBS

import Thentos.Sybil.Captcha
import Thentos.Types


spec :: Spec
spec = describe "Thentos.Sybil.Captcha" $ do
    it "produces three-word phrases as solutions" $ do
        let Just x = generateCaptcha <$> mkRandom20 "--------------------"
        (length . words . cs . snd $ x) `shouldBe` 3

    it "different rnd seed produces different solutions" $ do
        let Just x = generateCaptcha <$> mkRandom20 "--------------------"
            Just y = generateCaptcha <$> mkRandom20 "---------------+----"
        snd x `shouldNotBe` snd y

    it "writes pngs" $ do
        (img, _) <- generateCaptcha <$> mkRandom20'
        previewImg img


mkRandom20' :: IO Random20
mkRandom20' = do
    seed <- sequence $ replicate 20 (getRandom :: IO Word8)
    case mkRandom20 $ SBS.pack seed of
        Just r  -> return r
        Nothing -> error "mkRandom20': unreached."

previewImg :: ImageData -> IO ()
previewImg (ImageData img) = do
    _ <- forkIO $ do
        _ <- system "killall feh 2>/dev/null"
        (i, _, _, _) <- runInteractiveCommand "feh -"
        SBS.hPutStr i img
        hFlush i
        hClose i
    SBS.length img `shouldNotBe` 0
