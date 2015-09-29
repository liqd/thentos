import Distribution.Simple
import Distribution.PackageDescription (HookedBuildInfo, emptyHookedBuildInfo)
import Distribution.Simple.Setup (BuildFlags)

import System.Process (callProcess)

buildJs :: Args -> BuildFlags -> IO HookedBuildInfo
buildJs _ _ = do
    callProcess "./build-js.sh" []
    return emptyHookedBuildInfo

main = defaultMainWithHooks $ simpleUserHooks { preBuild = buildJs }
