{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ViewPatterns         #-}

module Thentos.Sybil.GraphicCaptcha (generateCaptcha) where

import Control.Monad.Random (MonadRandom, StdGen, mkStdGen, evalRand)
import Control.Monad (replicateM, forM_)
import Data.Char (ord)
import Data.Elocrypt (mkPassword)
import Data.String.Conversions (SBS, ST, cs)

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
    return $ (`evalRand` (random20ToStdGen rnd)) $ do
        solution <- mkSolution
        let challenge = mkChallenge font solution
        return (challenge, solution)


random20ToStdGen :: Random20 -> StdGen
random20ToStdGen = mkStdGen . sum . map ord . cs . fromRandom20


mkSolution :: MonadRandom m => m ST
mkSolution = cs . unwords <$> replicateM 2 (mkPassword 3)


type Pix = PixelRGBA8

mkChallenge :: Font -> ST -> ImageData
mkChallenge font solution = 
    let w = 500
        h = 100
        chars = cs solution :: String
        render = renderDrawing w h (PixelRGBA8 255 255 255 255)
    in ImageData $ cs $ encodePng $ render $ do
        let offsets = [20,70..]
        forM_ (zip offsets chars) $ \(offset, char) -> do
             biteLetter font 40 (V2 offset 20) char


biteLetter :: Font -> Float -> Point -> Char -> Drawing Pix ()
biteLetter font size point@(V2 l t) char = do
    let txBlack = uniformTexture black
        pText = printTextAt font (PointSize size) (V2 0 60) [char]
        letter = withTexture txBlack pText
        txLetter = patternTexture 100 100 96 white letter
        rect = rectangle point size 50
        shift = transformTexture $ translate (V2 (-l) 0)
        biteOffset = (V2 10 10)
        bite = rectangle (point ^-^ biteOffset) 32 32
        biteShift = transformTexture $ translate (V2 (-l) 0 ^+^ biteOffset)
    -- Draw a box around the letter
    --withTexture txBlack $ stroke 1 JoinRound (CapRound, CapRound) rect
    withTexture (shift txLetter) $ fill rect
    --withTexture (biteShift $ uniformTexture color1) fill bite
    withTexture (biteShift txLetter) $ fill bite


black = PixelRGBA8 0 0 0 255
white = PixelRGBA8 255 255 255 255
color1 = PixelRGBA8 0 0 255 255
color2 = PixelRGBA8 0 255 0 255
