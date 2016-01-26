module Network.HostAddrSpec where

import Test.Hspec (Spec, describe, it, shouldBe, shouldThrow, anyIOException)
import Network.HostAddr

spec :: Spec
spec = describe "Network.HostAddr" $ do
    describe "getHostAddr" $ do
        let f x y = do
                h <- getHostAddr x
                show h `shouldBe` y
        it "works." $ do
          f "127.0.0.1" "HostAddress 16777343"
          f "::1" "HostAddress6 (0,0,0,1)"
          f "255.255.255.255" "HostAddress 4294967295"
          f "ff:ff:ff:ff:ff:ff:ff:ff" "HostAddress6 (16711935,16711935,16711935,16711935)"
          shouldThrow (getHostAddr "1.2.3.4.5") anyIOException
          shouldThrow (getHostAddr "ff:ff:ff:ff:ff:ff:ff:ff:ff") anyIOException
          shouldThrow (getHostAddr "example.com") anyIOException
