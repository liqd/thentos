module MetaSpec where

import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Test.Hspec (Spec, describe, it, before_, shouldBe, runIO)

-- `before` and `after` hooks are run before and after each spec item (see
-- http://hspec.github.io/writing-specs.html ). Illustrative example:

spec :: Spec
spec = do
    ref <- runIO (newIORef 123)
    describe "all the tests" . before_ (beforeAction ref) $ do
        spec_foo ref

beforeAction :: IORef Int -> IO ()
beforeAction ref = writeIORef ref 0

spec_foo :: IORef Int -> Spec
spec_foo ref = describe "do the test" $ do
    it "first spec item modifies environment" $ do
        cur <- readIORef ref
        cur `shouldBe` 0
        modifyIORef ref (+ 1)
    it "environment gets cleaned up before second test item" $ do
        cur <- readIORef ref
        cur `shouldBe` 0
