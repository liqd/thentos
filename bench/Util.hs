module Util (timed) where

import Control.Applicative ((<$>))
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Exception (finally)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)

timed :: IO a -> IO (a,Double)
timed act = do
  t <- newEmptyMVar
  startWall <- getPOSIXTime
  ret <- act `finally` do
    endWall <- getPOSIXTime
    let elapsedWall = realToFrac $ endWall - startWall
    putMVar t elapsedWall
  ((,) ret) <$> takeMVar t
