{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TypeFamilies                             #-}
{-# LANGUAGE TypeOperators                            #-}

{-# OPTIONS -fno-warn-orphans #-}

module Thentos.Backend.Api.Docs.Proxy where

import Control.Lens ((&), (%~))
import Data.Proxy (Proxy(Proxy))
import Servant.API ((:<|>))
import Servant.Docs (HasDocs(..))

import qualified Servant.Docs as Docs
import qualified Servant.Foreign as F

import Thentos.Backend.Api.Docs.Common ()
import Thentos.Backend.Api.Proxy (ServiceProxy)


instance HasDocs sublayout => HasDocs (sublayout :<|> ServiceProxy) where
    docsFor _ dat opt = docsFor (Proxy :: Proxy sublayout) dat opt
                        & Docs.apiIntros %~ (++ intros)
      where
        intros = [Docs.DocIntro "@@1.3@@Authenticating Proxy" [unlines desc]]
        desc = [ "All requests that are not handled by the endpoints listed"
               , "below are handled as follows:"
               , "We extract the Thentos Session Token (X-Thentos-Session) from"
               , "the request headers and forward the request to the service, adding"
               , "X-Thentos-User and X-Thentos-Groups with the appropriate"
               , "data to the request headers. If the request does not include"
               , "a valid session token, it is rejected. Responses from the"
               , "service are returned unmodified."
               ]

instance F.HasForeign F.NoTypes ServiceProxy where
    type Foreign ServiceProxy = F.Req
    foreignFor Proxy Proxy req =
        req & F.funcName %~ ("ServiceProxy" :)
