import Test.Hspec

-- We used hspec-discover to create this module based on all modules
-- named '*Spec' under this directory, but we want to make the tests
-- depending on selenium grid optional.  We could use CPP, or set up
-- two directory trees, one for hspec-discover and one for more
-- complicated things like optional tests.  But for the sake of
-- simplicty, we just have a manually maintained list now.
--
-- See also:
-- https://github.com/hspec/hspec/issues/225
-- https://github.com/liqd/thentos/pull/10

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
        "--with-selenium" : xs -> withArgs xs $ hspec (spec True)
        _ -> hspec $ spec False

spec :: Bool -> Spec
spec browserTests = do
    describe "Thentos" ThentosSpec.spec
    describe "Thentos.Types" Thentos.TypesSpec.spec
    describe "Thentos.Backend.Api.Adhocracy3" Thentos.Backend.Api.Adhocracy3Spec.spec
    describe "Thentos.Backend.Api.Simple" Thentos.Backend.Api.SimpleSpec.spec
    when browserTests $ describe "Thentos.Frontend" Thentos.FrontendSpec.spec
