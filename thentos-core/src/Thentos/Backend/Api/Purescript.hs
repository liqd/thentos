{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE OverloadedStrings                        #-}

module Thentos.Backend.Api.Purescript where  -- FIXME: rename to "PureScript" (like the purescript hackage package)

import Data.Configifier ((>>.))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (cs)
import Servant.API (Raw)
import Servant.Server.Internal.RoutingApplication (toApplication, RouteResult(Fail))
import Servant.Server.Internal.ServantErr (err404, errBody)
import Servant.Server (Server)
import Servant.Utils.StaticFiles (serveDirectory)

import Thentos.Config (ThentosConfig)


type Api = Raw

api :: ThentosConfig -> Server Api
api cfg = api' $ cs <$> cfg >>. (Proxy :: Proxy '["purescript"])

api' :: Maybe FilePath -> Server Api
api' (Just fp) = serveDirectory fp
api' Nothing = toApplication $
      \_ cont -> cont $ Fail err404 { errBody = "purescript frontend not configured." }
