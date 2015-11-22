--
-- DO NOT EDIT!  THIS IS GENERATED REST API CLIENT CODE!
--
-- source package: thentos-adhocracy
-- source package version: Version {versionBranch = [0,0,1,1], versionTags = []}
--
module Adhocracy3 where

import Prelude
import Data.Foreign
import Data.Maybe
import Network.HTTP.Affjax
import Network.HTTP.Method
import Network.HTTP.RequestHeader
import Util (encodeURIComponent)

post200PrincipalsUsers :: forall eff. String -> Affjax eff Foreign
post200PrincipalsUsers body = affjax $ defaultRequest
    { method = POST
    , url = "/principals/users"
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json"]
    , content = Just body
    }

post200Activate_account :: forall eff. String -> Affjax eff Foreign
post200Activate_account body = affjax $ defaultRequest
    { method = POST
    , url = "/activate_account"
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json"]
    , content = Just body
    }

post200Login_username :: forall eff. String -> Affjax eff Foreign
post200Login_username body = affjax $ defaultRequest
    { method = POST
    , url = "/login_username"
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json"]
    , content = Just body
    }

post200Login_email :: forall eff. String -> Affjax eff Foreign
post200Login_email body = affjax $ defaultRequest
    { method = POST
    , url = "/login_email"
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json"]
    , content = Just body
    }

post200Password_reset :: forall eff. String -> Affjax eff Foreign
post200Password_reset body = affjax $ defaultRequest
    { method = POST
    , url = "/password_reset"
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json"]
    , content = Just body
    }

ServiceProxy :: forall eff. Affjax eff Foreign
ServiceProxy = affjax $ defaultRequest
    { method = GET
    , url = "/"
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json"]
    }

