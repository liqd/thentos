{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE ViewPatterns         #-}

-- | This is a port of https://hackage.haskell.org/package/hs-captcha (which is based on
-- http://libgd.github.io/) to diagrams.  The generated strings are beautified with elocrypt.
-- See also: https://github.com/liqd/thentos/blob/master/docs/sybil.md
--
-- FIXME: use 'Audio Word16' from package HCodecs instead of SBS.
-- FIXME: provide start-time check (`init` function) for existence of executables.  use it in main.
-- FIXME: this module could easily come in its own package, independent of the rest of the
--        thentos-core code.
module Thentos.Sybil.Captcha (generateCaptcha, generateAudioCaptcha) where

import Codec.Picture (encodePng)
import Control.Monad.Except (throwError)
import Control.Monad.Random (getRandomR, MonadRandom, StdGen, mkStdGen, evalRand)
import Control.Monad (replicateM, unless)
import Data.Char (ord)
import Data.Elocrypt (mkPassword)
import Data.String.Conversions (ST, SBS, cs)
import Diagrams.Backend.Rasterific (Rasterific(..), Options(RasterificOptions))
import Diagrams.Prelude hiding (ImageData)
import Graphics.SVGFonts (lin2, textSVG', TextOpts(TextOpts), Mode(INSIDE_H), Spacing(KERN))
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (runInteractiveProcess, waitForProcess)

import qualified Data.Text as ST
import qualified Data.ByteString as SBS

import Thentos.Types
import Thentos.Action.Core
import Thentos.Action.Unsafe (unsafeLiftIO)


-- * visual

-- | Generate a captcha. Returns a pair of the binary image data in PNG format and the correct
-- solution to the captcha.
generateCaptcha :: Random20 -> (ImageData, ST)
generateCaptcha rnd = (`evalRand` random20ToStdGen rnd) $ do
    solution <- mkSolution
    challenge <- mkChallenge solution
    return (challenge, solution)

random20ToStdGen :: Random20 -> StdGen
random20ToStdGen = mkStdGen . sum . map ord . cs . fromRandom20

mkSolution :: MonadRandom m => m ST
mkSolution = cs . unwords <$> replicateM 3 (mkPassword 4)
  -- Use 'mkPassphrase' here once https://github.com/sgillespie/elocrypt/pull/5 is on hackage.  Do
  -- not use `mkPasswords` unless https://github.com/sgillespie/elocrypt/pull/6 has been addressed.

type Dg = QDiagram Rasterific V2 Double Any

mkChallenge :: MonadRandom m => ST -> m ImageData
mkChallenge solution = ImageData . cs . encodePng . renderDia Rasterific opts
                    <$> (distortChallenge . text' . cs $ solution)
  where
    opts :: Options Rasterific V2 Double
    opts = RasterificOptions spec

    spec :: SizeSpec V2 Double
    spec = mkWidth 200

text' :: String -> Dg
text' s = strokeP (textSVG' (TextOpts lin2 INSIDE_H KERN False 1 1) s)
        # lw none # fc white # bg purple

distortChallenge :: forall m. MonadRandom m => Dg -> m Dg
distortChallenge dg = mconcat <$> sequence [someCircle, someCircle, someCircle, pure dg]

someCircle :: MonadRandom m => m Dg
someCircle = do
    ra <- abs    <$> go
    rt <- (+0.5) <$> go
    x  <- (*4)   <$> go
    y  <- (*0.2) <$> go
    circle ra
      # rotateBy rt
      # translate (r2 (x, y))
      # lw veryThick # lc white # dashingG [0.4, 0.1] 0
      # return
  where
    go :: MonadRandom m => m Double
    go = getRandomR (-0.5, 0.5)


{-

-- FIXME: the hs-captcha code for mining:

module Graphics.Captcha where

import Data.ByteString
import Data.Char
import Graphics.GD
import System.Random


main :: IO ()
main = do
  string <- makeRandomString
  image <- createInitialImage string
  chirpDoubleRandom image
  chirpDoubleRandom image
  image <- cropToFinalSize image
  Prelude.putStrLn string
  savePngFile "captcha.png" image


makeCaptcha :: IO (String, ByteString)
makeCaptcha = do
  string <- makeRandomString
  image <- createInitialImage string
  chirpDoubleRandom image
  chirpDoubleRandom image
  image <- cropToFinalSize image
  byteString <- savePngByteString image
  return (string, byteString)


makeRandomString :: IO String
makeRandomString = do
  let makeRandomLetter = do
                     n <- randomRIO (0, 25)
                     return $ chr (ord 'A' + n)
  mapM (\_ -> makeRandomLetter) [0..5]


chirpDoubleRandom :: Image -> IO ()
chirpDoubleRandom image = do
  depth1 <- randomRIO (2.0, 5.0)
  period1 <- randomRIO (1, 6)
  period2 <- randomRIO (1, 6)
  depth2 <- randomRIO (2.0, 5.0)
  period3 <- randomRIO (1, 6)
  period4 <- randomRIO (1, 6)
  chirpVertically (makeChirpFunction depth1
                                     (fromIntegral captchaSize / period1)
                                     (fromIntegral captchaSize / period2))
                  image
  chirpHorizontally (makeChirpFunction depth2
                                       (fromIntegral captchaSize / period3)
                                       (fromIntegral captchaSize / period4))
                    image


chirpHorizontally :: (Int -> Int) -> Image -> IO ()
chirpHorizontally chirpFunction image = do
  withImage (newImage (captchaSize, captchaSize))
            (\temporaryImage -> do
               copyRegion (0, 0) (captchaSize, captchaSize) image
                          (0, 0) temporaryImage
               let shiftRow row amount
                       = copyRegion (0, row) (captchaSize, 1) image
                                    (amount, row) temporaryImage
                   shiftRows function
                       = shiftRows' 0 function
                   shiftRows' i function
                       = if i == captchaSize
                            then return ()
                            else do
                              shiftRow i (function i)
                              shiftRows' (i+1) function
               shiftRows chirpFunction
               copyRegion (0, 0) (captchaSize, captchaSize) temporaryImage
                          (0, 0) image)


chirpVertically :: (Int -> Int) -> Image -> IO ()
chirpVertically chirpFunction image = do
  withImage (newImage (captchaSize, captchaSize))
            (\temporaryImage -> do
               copyRegion (0, 0) (captchaSize, captchaSize) image
                          (0, 0) temporaryImage
               let shiftColumn column amount
                       = copyRegion (column, 0) (1, captchaSize) image
                                    (column, amount) temporaryImage
                   shiftColumns function
                       = shiftColumns' 0 function
                   shiftColumns' i function
                       = if i == captchaSize
                            then return ()
                            else do
                              shiftColumn i (function i)
                              shiftColumns' (i+1) function
               shiftColumns chirpFunction
               copyRegion (0, 0) (captchaSize, captchaSize) temporaryImage
                          (0, 0) image)


makeChirpFunction :: Float -> Float -> Float -> Int -> Int
makeChirpFunction depth startingWaveLength endingWaveLength row
    = let waveLength = ((startingWaveLength * fromIntegral row)
                        + (endingWaveLength * fromIntegral (captchaSize - row)))
                       / fromIntegral captchaSize
      in floor $ depth * (sin $ (fromIntegral row) * ((2*pi) / waveLength))


createInitialImage :: String -> IO Image
createInitialImage string = do
  image <- newImage (captchaSize, captchaSize)
  useFontConfig True
  drawFilledRectangle (0, 0) (captchaSize, captchaSize) 0xFFFFFF image
  ((left, top), _, (right, bottom), _)
      <- measureString fontName fontSize 0.0 (0, 0) string 0x000000
  width <- return $ right - left
  height <- return $ top - bottom
  originX <- return $ (captchaSize - width) `div` 2
  originY <- return $ (captchaSize + height) `div` 2
  drawString fontName fontSize 0.0 (originX, originY) string 0x000000 image
  return image


cropToFinalSize :: Image -> IO Image
cropToFinalSize image = do
  result <- newImage finalSize
  copyRegion ((captchaSize - fst finalSize) `div` 2,
              (captchaSize - snd finalSize) `div` 2)
             finalSize
             image
             (0, 0)
             result
  return result


fontName :: String
fontName = "Courier New"


fontSize :: Double
fontSize = 22.0


captchaSize :: Int
captchaSize = 192


finalSize :: (Int, Int)
finalSize = (128, 64)

-}


-- * audio

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

mkAudioChallenge :: forall e s. String -> ST -> Action e s SBS
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
