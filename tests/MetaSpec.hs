module MetaSpec where

import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Test.Hspec (Spec, describe, it, before_, shouldBe, runIO, hspec)

tests :: IO ()
tests = hspec spec

spec :: Spec
spec = hooks


-- `before` and `after` hooks are run before and after each spec item (see
-- http://hspec.github.io/writing-specs.html). Illustrative example:

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
