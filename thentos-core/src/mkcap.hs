
module Mkcap (img) where

import Control.Monad (forM_)
import Data.ByteString as BS
import Data.String.Conversions
import Thentos.Sybil
import Thentos.Types


img :: IO ()
img = do
    let baseNum = 11234567890123456789
    forM_ [0..20] $ \n -> do
        let Just rnd = mkRandom20 $ cs $ show $ baseNum + n
        (ImageData pngData, solution) <- generateCaptcha rnd
        let path = "/host/tmp/" ++ cs solution ++ ".png"
        BS.writeFile path pngData


