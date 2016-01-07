{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ViewPatterns         #-}

-- FIXME: use 'Audio Word16' from package HCodecs instead of SBS.
-- FIXME: provide start-time check (`init` function) for existence
-- of executables.  use it in main.

module Thentos.Sybil.AudioCaptcha (generateAudioCaptcha) where

import Control.Monad.Except (throwError)
import Control.Monad (unless)
import Data.Char (ord)
import Data.String.Conversions (ST, SBS, cs)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (runInteractiveProcess, waitForProcess)

import qualified Data.Text as ST
import qualified Data.ByteString as SBS

import Thentos.Types
import Thentos.Action.Types
import Thentos.Action.Unsafe (unsafeLiftIO)


-- | Generate a captcha. Returns a pair of the binary audio data in WAV format and the correct
-- solution to the captcha.  (Return value is wrapped in 'Action' for access to 'IO' and for
-- throwing 'ThentosError'.)
generateAudioCaptcha :: String -> Random20 -> Action e s (SBS, ST)
generateAudioCaptcha eSpeakVoice rnd = do
    let solution = mkAudioSolution rnd
    challenge <- mkAudioChallenge eSpeakVoice solution
    return (challenge, solution)

-- | Returns 6 digits of randomness.  (This loses a lot of the input randomness, but captchas are a
-- low-threshold counter-measure, so they should be at least reasonably convenient to use.)
--
-- FIXME: use nato spelling alphabet on the letters from the visual challenge instead.  makes for a
-- better UI *and* slightly better security.
mkAudioSolution :: Random20 -> ST
mkAudioSolution = ST.intercalate " "
                . ((cs . show . (`mod` 10) . ord <$>) :: String -> [ST])
                . take 6 . cs . fromRandom20

mkAudioChallenge :: String -> ST -> Action e s SBS
mkAudioChallenge eSpeakVoice solution = do
    unless (validateLangCode eSpeakVoice) $ do
        throwError $ AudioCaptchaVoiceNotFound eSpeakVoice

    eResult <- unsafeLiftIO . withSystemTempDirectory "thentosAudioCaptcha" $
      \((</> "captcha.wav") -> tempFile) -> do
        let args :: [String]
            args = [ "-w", tempFile
                       -- FIXME: use "--stdout" (when I tried, I had truncation issues)
                   , "-s", "100"
                   , "-v", eSpeakVoice
                   , cs $ ST.intersperse ' ' solution
                   ]
        (_, outH, errH, procH) <- runInteractiveProcess "espeak" args Nothing Nothing
        outS <- SBS.hGetContents outH
        errS <- SBS.hGetContents errH
        exitCode <- waitForProcess procH

        if "Failed to read voice" `SBS.isInfixOf` errS
            then return . Left $ AudioCaptchaVoiceNotFound eSpeakVoice
            else if not ((exitCode == ExitSuccess) && SBS.null outS && SBS.null errS)
                then return . Left $ AudioCaptchaInternal exitCode outS errS
                else Right <$> SBS.readFile tempFile

    case eResult of
        Left e -> throwError e
        Right v -> return v

validateLangCode :: String -> Bool
validateLangCode (cs -> s) =
    not (ST.null s || ST.isPrefixOf "-" s || ST.isSuffixOf "-" s)
    && ST.all (`elem` '-':['a'..'z']) s
