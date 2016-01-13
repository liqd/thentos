{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Thentos.Sybil.GraphicCaptcha (generateCaptcha) where

import Control.Exception (throwIO, ErrorCall(ErrorCall))
import Control.Monad.Except (replicateM, forM_, forM)
import Control.Monad.Random.Class (MonadRandom, getRandomR)
import Control.Monad.Random (mkStdGen, evalRand)
import Data.Char (ord)
import Data.Elocrypt (mkPassword)
import Data.String.Conversions (ST, cs)

import Codec.Picture (PixelRGBA8(PixelRGBA8), encodePng)
import Graphics.Rasterific
    ( Drawing, PointSize(PointSize), Point
    , withTransformation, withTexture, printTextAt, rectangle, renderDrawing, fill
    )
import Graphics.Rasterific.Linear (V2(V2), (^*))
import Graphics.Rasterific.Texture (uniformTexture, patternTexture, transformTexture)
import Graphics.Rasterific.Transformations (translate)
import Graphics.Text.TrueType (Font, loadFontFile)

import Paths_thentos_core__
import Thentos.Types


-- | Generate a captcha. Returns a pair of the binary image data in PNG format and the correct
-- solution to the captcha.
generateCaptcha :: Random20 -> IO (ImageData, ST)
generateCaptcha rnd = do
    fontPath <- getDataFileName "resources/fonts/Courier_Prime_Bold.ttf"
    font <- loadFontFile fontPath >>= either (throwIO . ErrorCall) return
    let random20ToStdGen = mkStdGen . sum . map ord . cs . fromRandom20
    return $ flip evalRand (random20ToStdGen rnd) $ do
        solution <- mkSolution
        challenge <- mkChallenge font solution
        return (challenge, cs solution)

mkSolution :: MonadRandom m => m String
mkSolution = unwords <$> replicateM 2 (mkPassword 3)


mkChallenge :: forall m. MonadRandom m => Font -> String -> m ImageData
mkChallenge font solution =
    let width = 300
        height = 100
        offsets = [30,65..]
        yOffset = 30
        textPx = 40
        render = renderDrawing width height (PixelRGBA8 255 255 255 255)
        letterParams = zip offsets solution

        fuzz :: Float -> m Float
        fuzz i = getRandomR (i * 0.7 + 0.2, i * 1.3 - 0.2)

        action :: m (Drawing PixelRGBA8 ())
        action = do
            offsetParams <- forM solution $ \_ -> do
                holeOffset@(V2 x y) <- V2 <$> fuzz 10 <*> fuzz 0
                chunkOffset         <- V2 <$> fuzz (-x) <*> fuzz (-5-y)
                return (holeOffset, chunkOffset)

            let allParams = zip letterParams offsetParams

            return $ forM_ allParams $ \((offset, char), (holeOffset, chunkOffset)) -> do
                withTransformation (translate $ V2 offset yOffset) $
                    biteLetter holeOffset chunkOffset font textPx char

    in ImageData . cs . encodePng . render <$> action


-- | Render a letter with a chunk displaced.
biteLetter :: Point -> Point -> Font -> Float -> Char -> Drawing PixelRGBA8 ()
biteLetter _ _ _ _ ' ' = return ()
biteLetter holeOffset chunkOffset font size char = do
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
    hole = rectangle holeOffset 30 30
    chunk = rectangle chunkOffset 30 30
    texChunk = transformTexture (translate $ chunkOffset ^* (-1)) texLetter
