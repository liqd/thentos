{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}

module Thentos.Sybil.CaptchaSpec where

import Codec.Picture (decodePng)
import Control.Concurrent (forkIO)
import Control.Monad.Random (getRandom)
import Control.Monad (replicateM, when, void)
import Data.Either (isRight)
import Data.String.Conversions (cs)
import Data.Word8 (Word8)
import System.IO (hFlush, hClose)
import System.Process (runInteractiveCommand, system)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

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
        previewImg False img
        (isRight . decodePng . fromImageData $ img) `shouldBe` True


mkRandom20' :: IO Random20
mkRandom20' = do
    seed <- replicateM 20 (getRandom :: IO Word8)
    case mkRandom20 $ SBS.pack seed of
        Just r  -> return r
        Nothing -> error "mkRandom20': unreached."

previewImg :: Bool -> ImageData -> IO ()
previewImg interactiveDevelopment (ImageData img) = do
    when interactiveDevelopment . void . forkIO $ do
        _ <- system "killall feh 2>/dev/null"
        (i, _, _, _) <- runInteractiveCommand "feh -"
        SBS.hPutStr i img
        hFlush i
        hClose i
    SBS.length img `shouldNotBe` 0
