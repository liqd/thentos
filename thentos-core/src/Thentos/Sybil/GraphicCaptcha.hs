{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ViewPatterns         #-}

module Thentos.Sybil.GraphicCaptcha (generateCaptcha) where

import Control.Monad.Random (MonadRandom, StdGen, mkStdGen, evalRand)
import Control.Monad (replicateM)
import Data.Char (ord)
import Data.Elocrypt (mkPassword)
import Data.String.Conversions (SBS, ST, cs)

import Codec.Picture( PixelRGBA8( .. ), encodePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Graphics.Text.TrueType (Font, loadFontFile)

import Paths_thentos_core__
import Thentos.Types


-- | Generate a captcha. Returns a pair of the binary image data in PNG format and the correct
-- solution to the captcha.
generateCaptcha :: Random20 -> IO (ImageData, ST)
generateCaptcha rnd = do
    fontPath <- getDataFileName "resources/fonts/Courier_Prime_Bold.ttf"
    font <- either error id <$> loadFontFile fontPath
    return $ (`evalRand` (random20ToStdGen rnd)) $ do
        solution <- mkSolution
        let challenge = mkChallenge font solution
        return (challenge, solution)


random20ToStdGen :: Random20 -> StdGen
random20ToStdGen = mkStdGen . sum . map ord . cs . fromRandom20


mkSolution :: MonadRandom m => m ST
mkSolution = cs . unwords <$> replicateM 2 (mkPassword 3)


mkChallenge :: Font -> ST -> ImageData
mkChallenge font solution = 
    let w = 400
        h = 70
        render = renderDrawing w h (PixelRGBA8 255 255 255 255)
        text = printTextAt font (PointSize 50) (V2 20 40) $ cs solution
        c1 = checker 10 10 (uniformTexture black) (uniformTexture white)
        c2 = checker 10 10 (uniformTexture color1) (uniformTexture color2)
        drawing = do withTexture c1 $ fill $ rectangle (V2 0 0) (fromIntegral w)
                                                                (fromIntegral h)
                     withTexture c2 text
    in ImageData $ cs $ encodePng $ render $ drawing


checker :: Int -> Int -> Texture PixelRGBA8 -> Texture PixelRGBA8 -> Texture PixelRGBA8
checker w' h' t1 t2 = patternTexture (w'*2) (h'*2) 96 black $ do
    withTexture t1 $ do fill $ rectangle (V2 0 0) w h
                        fill $ rectangle (V2 w h) w h
    withTexture t2 $ do fill $ rectangle (V2 w 0) w h
                        fill $ rectangle (V2 0 h) w h
  where
    w = fromIntegral w'
    h = fromIntegral h'


black = PixelRGBA8 0 0 0 255
white = PixelRGBA8 255 255 255 255
color1 = PixelRGBA8 0 0 255 255
color2 = PixelRGBA8 0 255 0 255
