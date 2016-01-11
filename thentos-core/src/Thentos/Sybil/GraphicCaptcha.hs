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
    solution = flip evalRand (random20ToStdGen rnd) mkSolution
    random20ToStdGen = mkStdGen . sum . map ord . cs . fromRandom20
    challenge font = mkChallenge font solution


mkSolution :: MonadRandom m => m String
mkSolution = unwords <$> replicateM 2 (mkPassword 3)


mkChallenge :: Font -> String -> ImageData
mkChallenge font solution =
    let width = 300
        height = 100
        offsets = [30,65..]
        yOffset = 30
        textPx = 40
        render = renderDrawing width height (PixelRGBA8 255 255 255 255)
        letterParams = zip offsets solution
    in ImageData $ cs $ encodePng $ render $
        forM_ letterParams $ \(offset, char) ->
             withTransformation (translate $ V2 offset yOffset) $
                 biteLetter font textPx char


-- | Render a letter with a chunk displaced.
biteLetter :: Font -> Float -> Char -> Drawing PixelRGBA8 ()
biteLetter _ _ ' ' = return ()
biteLetter font size char = do
    withTexture texLetter $ fill rect
    withTexture texWhite $ fill hole
    withTexture texChunk $ fill chunk
  where
    -- Render letter to texture
    black = PixelRGBA8 0 0 0 255
    white = PixelRGBA8 255 255 255 255
    transparent = PixelRGBA8 255 255 255 0
    texBlack = uniformTexture black
    texWhite = uniformTexture white
    printText = printTextAt font (PointSize size) (V2 0 size) [char]
    letter = withTexture texBlack printText
    texLetter = patternTexture 100 100 96 transparent letter
    rect = rectangle (V2 0 0) size 50
    -- Take a chunk of the letter and displace it randomly
    holeOffset = V2 10 0
    hole = rectangle holeOffset 30 30
    chunkOffset = V2 (-10) (-5)
    chunk = rectangle chunkOffset 30 30
    texChunk = transformTexture (translate $ chunkOffset ^* (-1)) texLetter
