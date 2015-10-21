
import Servant
import Text.Blaze.Html5 as H

module Thentos.FrontendS where

import Thentos.Frontend.HandlersS

type FrontendAPI = "user" :> UserAPI

type UserAPI = "register" :> (Get '[HTML] H.Html :<|> Post '[FormUrlEncoded] H.Html)


frontendServer :: Server FrontendAPI
frontendServer = registerServer
