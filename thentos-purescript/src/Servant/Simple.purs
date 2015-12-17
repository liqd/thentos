--
-- DO NOT EDIT!  THIS IS GENERATED REST API CLIENT CODE!
--
-- source package: thentos-core
-- source package version: Version {versionBranch = [], versionTags = []}
--
module Servant.Simple where

import Prelude
import Data.Foreign
import Data.Maybe
import Network.HTTP.Affjax
import Network.HTTP.Method
import Network.HTTP.RequestHeader
import Util (encodeURIComponent)

postUser :: forall eff. String -> String -> Affjax eff Foreign
postUser body headerxthentossession = affjax $ defaultRequest
    { method = POST
    , url = "/user"
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json", RequestHeader "x-thentos-session" headerxthentossession]
    , content = Just body
    }

postUserRegister :: forall eff. String -> String -> Affjax eff Foreign
postUserRegister body headerxthentossession = affjax $ defaultRequest
    { method = POST
    , url = "/user/register"
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json", RequestHeader "x-thentos-session" headerxthentossession]
    , content = Just body
    }

postUserActivate :: forall eff. String -> String -> Affjax eff Foreign
postUserActivate body headerxthentossession = affjax $ defaultRequest
    { method = POST
    , url = "/user/activate"
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json", RequestHeader "x-thentos-session" headerxthentossession]
    , content = Just body
    }

postUserLogin :: forall eff. String -> String -> Affjax eff Foreign
postUserLogin body headerxthentossession = affjax $ defaultRequest
    { method = POST
    , url = "/user/login"
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json", RequestHeader "x-thentos-session" headerxthentossession]
    , content = Just body
    }

deleteUserByUidWithUid :: forall eff. String -> String -> Affjax eff Foreign
deleteUserByUidWithUid uid headerxthentossession = affjax $ defaultRequest
    { method = DELETE
    , url = "/user/" <> encodeURIComponent uid <> ""
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json", RequestHeader "x-thentos-session" headerxthentossession]
    }

getUserByUidNameWithUid :: forall eff. String -> String -> Affjax eff Foreign
getUserByUidNameWithUid uid headerxthentossession = affjax $ defaultRequest
    { method = GET
    , url = "/user/" <> encodeURIComponent uid <> "/name"
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json", RequestHeader "x-thentos-session" headerxthentossession]
    }

getUserByUidEmailWithUid :: forall eff. String -> String -> Affjax eff Foreign
getUserByUidEmailWithUid uid headerxthentossession = affjax $ defaultRequest
    { method = GET
    , url = "/user/" <> encodeURIComponent uid <> "/email"
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json", RequestHeader "x-thentos-session" headerxthentossession]
    }

postUserCaptcha :: forall eff. String -> Affjax eff Foreign
postUserCaptcha headerxthentossession = affjax $ defaultRequest
    { method = POST
    , url = "/user/captcha"
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json", RequestHeader "x-thentos-session" headerxthentossession]
    }

postService :: forall eff. String -> String -> Affjax eff Foreign
postService body headerxthentossession = affjax $ defaultRequest
    { method = POST
    , url = "/service"
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json", RequestHeader "x-thentos-session" headerxthentossession]
    , content = Just body
    }

deleteServiceBySidWithSid :: forall eff. String -> String -> Affjax eff Foreign
deleteServiceBySidWithSid sid headerxthentossession = affjax $ defaultRequest
    { method = DELETE
    , url = "/service/" <> encodeURIComponent sid <> ""
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json", RequestHeader "x-thentos-session" headerxthentossession]
    }

getService :: forall eff. String -> Affjax eff Foreign
getService headerxthentossession = affjax $ defaultRequest
    { method = GET
    , url = "/service"
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json", RequestHeader "x-thentos-session" headerxthentossession]
    }

postThentos_session :: forall eff. String -> String -> Affjax eff Foreign
postThentos_session body headerxthentossession = affjax $ defaultRequest
    { method = POST
    , url = "/thentos_session"
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json", RequestHeader "x-thentos-session" headerxthentossession]
    , content = Just body
    }

getThentos_session :: forall eff. String -> String -> Affjax eff Foreign
getThentos_session body headerxthentossession = affjax $ defaultRequest
    { method = GET
    , url = "/thentos_session"
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json", RequestHeader "x-thentos-session" headerxthentossession]
    , content = Just body
    }

deleteThentos_session :: forall eff. String -> String -> Affjax eff Foreign
deleteThentos_session body headerxthentossession = affjax $ defaultRequest
    { method = DELETE
    , url = "/thentos_session"
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json", RequestHeader "x-thentos-session" headerxthentossession]
    , content = Just body
    }

getService_session :: forall eff. String -> String -> Affjax eff Foreign
getService_session body headerxthentossession = affjax $ defaultRequest
    { method = GET
    , url = "/service_session"
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json", RequestHeader "x-thentos-session" headerxthentossession]
    , content = Just body
    }

getService_sessionMeta :: forall eff. String -> String -> Affjax eff Foreign
getService_sessionMeta body headerxthentossession = affjax $ defaultRequest
    { method = GET
    , url = "/service_session/meta"
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json", RequestHeader "x-thentos-session" headerxthentossession]
    , content = Just body
    }

deleteService_session :: forall eff. String -> String -> Affjax eff Foreign
deleteService_session body headerxthentossession = affjax $ defaultRequest
    { method = DELETE
    , url = "/service_session"
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json", RequestHeader "x-thentos-session" headerxthentossession]
    , content = Just body
    }

