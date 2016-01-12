{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}

module Thentos.Sybil.CaptchaSpec where

import Codec.ByteString.Parser (runParser)
import Codec.Picture (decodePng)
import Codec.Wav (parseWav)
import Control.Concurrent (forkIO)
import Control.Monad.Random (getRandom)
import Control.Monad (replicateM, when, void)
import Data.Audio (Audio)
import Data.Either (isRight)
import Data.String.Conversions (SBS, ST, cs)
import Data.Void (Void)
import Data.Word8 (Word8)
import System.Exit (ExitCode(ExitSuccess))
import System.IO (hFlush, hClose)
import System.Process (runInteractiveCommand, system, waitForProcess)
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe, shouldThrow)

import qualified Data.ByteString as SBS

import Thentos.Action.Core
import Thentos.Action.Types
import Thentos.Sybil
import Thentos.Test.Config (thentosTestConfig)
import Thentos.Test.Core (createActionState)
import Thentos.Types


spec :: Spec
spec = describe "Thentos.Sybil.Captcha" $ do
    it "produces three-word phrases as solutions" $ do
        let Just r = mkRandom20 "--------------------"
        x <- generateCaptcha r
        (length . words . cs . snd $ x) `shouldBe` 3

    it "different rnd seed produces different solutions" $ do
        let Just rx = mkRandom20 "--------------------"
            Just ry = mkRandom20 "---------------+----"
        x <- generateCaptcha rx
        y <- generateCaptcha ry
        snd x `shouldNotBe` snd y

    describe "mkChallenge" $ do
        it "writes pngs" $ do
            (img, _) <- mkRandom20' >>= generateCaptcha
            previewImg False img
            (isRight . decodePng . fromImageData $ img) `shouldBe` True

    describe "mkAudioChallenge" $ do
        it "writes wavs" $ do
            (wav, _) <- mkRandom20' >>= generateAudioCaptcha' "en"
            previewWav False wav
            isRight (runParser parseWav . cs $ wav :: Either String (Audio Word8)) `shouldBe` True

        let isNotFound :: ActionError Void -> Bool
            isNotFound (ActionErrorThentos (AudioCaptchaVoiceNotFound _)) = True
            isNotFound _ = False

        it "returns 404 on valid but unknown language key" $ do
            (mkRandom20' >>= generateAudioCaptcha' "no-such-voice-796")
                `shouldThrow` isNotFound

        it "returns 404 on code injection" $ do
            rnd <- mkRandom20'
            timeout (5 * 1000 * 1000) (generateAudioCaptcha' "`sleep 3600`" rnd)
                `shouldThrow` isNotFound


generateAudioCaptcha' :: String -> Random20 -> IO (SBS, ST)
generateAudioCaptcha' voice rnd = fst <$> do
    as <- createActionState "test_thentos" thentosTestConfig
    runAction () as $ (generateAudioCaptcha voice rnd :: Action Void () (SBS, ST))

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

previewWav :: Bool -> SBS -> IO ()
previewWav interactiveDevelopment raw = do
    case runParser parseWav . cs $ raw :: Either String (Audio Word8) of
        Right _ -> return ()
        Left e -> error $ "previewWav: parse error: " ++ show e
    when interactiveDevelopment . void . forkIO $ do
        (i, _, _, pid) <- runInteractiveCommand "play -"
        SBS.hPutStr i raw
        hFlush i
        ExitSuccess <- waitForProcess pid
        return ()
    SBS.length raw `shouldNotBe` 0
