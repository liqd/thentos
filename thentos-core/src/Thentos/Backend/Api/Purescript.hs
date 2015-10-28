{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE TypeOperators                            #-}

module Thentos.Backend.Api.Purescript where

import Servant.API (Raw)
import Servant.Server.Internal.RoutingApplication (toApplication, RouteResult(Fail))
import Servant.Server.Internal.ServantErr (err404, errBody)
import Servant.Server (Server)
import Servant.Utils.StaticFiles (serveDirectory)


type Api = Raw

api :: Maybe FilePath -> Server Api
api (Just fp) = serveDirectory fp
api Nothing = toApplication $
      \_ cont -> cont $ Fail err404 { errBody = "purescript frontend not configured." }
