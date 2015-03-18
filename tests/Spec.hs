import Test.Hspec

import Control.Monad (when)
import System.Environment (getArgs, withArgs)

import qualified ThentosSpec
import qualified Thentos.Backend.Api.Adhocracy3Spec
import qualified Thentos.Backend.Api.SimpleSpec
import qualified Thentos.FrontendSpec
import qualified Thentos.TypesSpec

main :: IO ()
main = do
    args <- getArgs
    case args of
        "enableBrowserTests" : xs -> withArgs xs $ hspec (spec True)
        _ -> hspec $ spec False

spec :: Bool -> Spec
spec browserTests = do
    describe "Thentos" ThentosSpec.spec
    describe "Thentos.Types" Thentos.TypesSpec.spec
    describe "Thentos.Backend.Api.Adhocracy3" Thentos.Backend.Api.Adhocracy3Spec.spec
    describe "Thentos.Backend.Api.Simple" Thentos.Backend.Api.SimpleSpec.spec
    when browserTests $ describe "Thentos.Frontend" Thentos.FrontendSpec.spec
