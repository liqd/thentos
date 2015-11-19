{-# LANGUAGE OverloadedStrings    #-}

-- | This is a port of https://hackage.haskell.org/package/hs-captcha (which is based on
-- http://libgd.github.io/) to diagrams.  The generated strings are beautified with elocrypt.
module Thentos.Sybil.Captcha where

import Control.Monad.Random (MonadRandom, StdGen, mkStdGen, evalRand)
import Data.Char (ord)
import Data.Elocrypt (mkPasswords)
import Data.String.Conversions (ST, cs)

import Thentos.Types


-- | Generate a captcha. Returns a pair of the binary image data in PNG format and the correct
-- solution to the captcha.
generateCaptcha :: Random20 -> (ImageData, ST)
generateCaptcha rnd = flip evalRand (random20ToStdGen rnd) $ do
    solution <- mkSolution
    challenge <- mkChallenge solution
    return (challenge, solution)

random20ToStdGen :: Random20 -> StdGen
random20ToStdGen = mkStdGen . sum . map ord . cs . fromRandom20

mkSolution :: MonadRandom m => m ST
mkSolution = cs . unwords . take 3 <$> mkPasswords 5

mkChallenge :: MonadRandom m => ST -> m ImageData
mkChallenge = error "mkChallenge"



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
