{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ViewPatterns         #-}

module Thentos.Sybil.GraphicCaptcha (generateCaptcha) where

import Control.Monad.Except
import Control.Monad.Random (mkStdGen, evalRand)
import Control.Monad.Random.Class
import Data.Char (ord)
import Data.Elocrypt (mkPassword)
import Data.String.Conversions (ST, cs)

import Codec.Picture( PixelRGBA8( .. ), encodePng )
import Graphics.Rasterific
import Graphics.Rasterific.Linear
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Transformations (translate)
import Graphics.Text.TrueType (Font, loadFontFile)

import Paths_thentos_core__
import Thentos.Types


-- | Generate a captcha. Returns a pair of the binary image data in PNG format and the correct
-- solution to the captcha.
generateCaptcha :: Random20 -> IO (ImageData, ST)
generateCaptcha rnd = do
    fontPath <- getDataFileName "resources/fonts/Courier_Prime_Bold.ttf"
    font <- either error id <$> loadFontFile fontPath
    return (challenge font, cs solution)
  where
    (solution, randoms) = flip evalRand (random20ToStdGen rnd)
        ((,) <$> mkSolution <*> getRandoms)
    random20ToStdGen = mkStdGen . sum . map ord . cs . fromRandom20
    challenge font = mkChallenge font randoms solution


mkSolution :: MonadRandom m => m String
mkSolution = unwords <$> replicateM 2 (mkPassword 3)


mkChallenge :: Font -> [Float] -> String -> ImageData
mkChallenge font rnds solution =
    let w = 500
        h = 100
        render = renderDrawing w h (PixelRGBA8 255 255 255 255)
        letterParams = zip3 [20,70..] solution rnds
    in ImageData $ cs $ encodePng $ render $
        forM_ letterParams $ \(offset, char, rnd) ->
             withTransformation (translate $ V2 offset 20) $
                 biteLetter font 40 rnd char


-- | Render a letter with a chunk displaced.
biteLetter :: Font -> Float -> Float -> Char -> Drawing PixelRGBA8 ()
biteLetter _ _ _ ' ' = return ()
biteLetter font size rnd char = do
    withTexture texLetter $ fill rect
    withTexture (biteShift texLetter) $ fill bite
  where
    -- Render letter to texture
    black = PixelRGBA8 0 0 0 255
    white = PixelRGBA8 255 255 255 255
    texBlack = uniformTexture black
    printText = printTextAt font (PointSize size) (V2 0 size) [char]
    letter = withTexture texBlack printText
    texLetter = patternTexture 100 100 96 white letter
    rect = rectangle (V2 0 0) size 50
    -- Take a chunk of the letter and displace it randomly
    radius = 20
    biteX = rnd * radius
    biteOffset = V2 biteX (radius - biteX)
    bite = rectangle biteOffset 20 20
    biteShift = transformTexture $ translate (biteOffset ^/ 2)

