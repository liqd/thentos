module MetaSpec where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import System.Exit (ExitCode(ExitSuccess))
import System.Process (system)
import Test.Hspec (Spec, describe, it, before_, shouldBe, runIO, hspec)

import Thentos.Test.Core
import Thentos.Test.Types


tests :: IO ()
tests = hspec spec

spec :: Spec
spec = hooks >> setupTestServerFullSpec


hooks :: Spec
hooks = do
    ref <- runIO (newIORef 123)
    describe "all the tests" . before_ (beforeAction ref) $ do
        specFoo ref

beforeAction :: IORef Int -> IO ()
beforeAction ref = writeIORef ref 0

specFoo :: IORef Int -> Spec
specFoo ref = describe "do the test" $ do
    it "first spec item modifies environment" $ do
        cur <- readIORef ref
        cur `shouldBe` 0
        modifyIORef ref (+ 1)
    it "environment gets cleaned up before second test item" $ do
        cur <- readIORef ref
        cur `shouldBe` 0


-- check that as soon as `setupTestServerFull`, two tcp listeners are running.

setupTestServerFullSpec :: Spec
setupTestServerFullSpec = describe "setupTestServerFull" . it "works" $ do
    fts <- liftIO setupTestServerFull
    let bport = show $ fts ^. (ftsCfg . tcfgServerFullBackendPort)
        fport = show $ fts ^. (ftsCfg . tcfgServerFullFrontendPort)

    bresult <- liftIO . system $ "curl http://localhost:" ++ bport ++ " > /dev/null 2>&1"
    bresult `shouldBe` ExitSuccess

    fresult <- liftIO . system $ "curl http://localhost:" ++ fport ++ " > /dev/null 2>&1"
    fresult `shouldBe` ExitSuccess

    liftIO $ teardownTestServerFull fts
