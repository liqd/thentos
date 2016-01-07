{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ViewPatterns         #-}

-- | This is a port of https://hackage.haskell.org/package/hs-captcha (which is based on
-- http://libgd.github.io/) to diagrams.  The generated strings are beautified with elocrypt.
-- See also: https://github.com/liqd/thentos/blob/master/docs/sybil.md
--
-- FIXME: use 'Audio Word16' from package HCodecs instead of SBS.
-- FIXME: provide start-time check (`init` function) for existence of executables.  use it in main.
-- FIXME: this module could easily come in its own package, independent of the rest of the
--        thentos-core code.
module Thentos.Sybil.GraphicCaptcha (generateCaptcha) where

import Control.Monad.Random (MonadRandom, StdGen, mkStdGen, evalRand)
import Control.Monad (replicateM)
import Data.Char (ord)
import Data.Elocrypt (mkPassword)
import Data.String.Conversions (ST, cs)

import Codec.Picture( PixelRGBA8( .. ), encodePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Graphics.Text.TrueType (Font, loadFontFile)

import Paths_thentos_core__
import Thentos.Types


-- * visual

-- | Generate a captcha. Returns a pair of the binary image data in PNG format and the correct
-- solution to the captcha.
generateCaptcha :: Random20 -> IO (ImageData, ST)
generateCaptcha rnd = do
    fontPath <- getDataFileName "resources/fonts/Courier_Prime.ttf"
    font <- either error id <$> loadFontFile fontPath
    return $ (`evalRand` (random20ToStdGen rnd)) $ do
        solution <- mkSolution
        challenge <- mkChallenge font solution
        return (challenge, solution)


random20ToStdGen :: Random20 -> StdGen
random20ToStdGen = mkStdGen . sum . map ord . cs . fromRandom20


mkSolution :: MonadRandom m => m ST
mkSolution = cs . unwords <$> replicateM 3 (mkPassword 4)


mkChallenge :: MonadRandom m => Font -> ST -> m ImageData
mkChallenge font solution = 
    let render = renderDrawing 300 70 (PixelRGBA8 255 255 255 255)
        text = printTextAt font (PointSize 12) (V2 20 40) $ cs solution
        textTexture = uniformTexture $ PixelRGBA8 0 0 0 255
        drawing = withTexture textTexture text
    in return $ ImageData $ cs $ encodePng $ render $ drawing

