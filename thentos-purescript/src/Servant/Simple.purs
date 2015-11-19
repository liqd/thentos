module Servant.Simple where

import Prelude
import Data.Foreign
import Data.Maybe
import Network.HTTP.Affjax
import Network.HTTP.Method
import Network.HTTP.RequestHeader
import Util (encodeURIComponent)

postUser :: forall eff. String -> Affjax eff Foreign
postUser body = affjax $ defaultRequest
    { method = POST
    , url = "/user"
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json"]
    , content = Just body
    }

postUserLogin :: forall eff. String -> Affjax eff Foreign
postUserLogin body = affjax $ defaultRequest
    { method = POST
    , url = "/user/login"
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json"]
    , content = Just body
    }

deleteUserByUidWithUid :: forall eff. String -> Affjax eff Foreign
deleteUserByUidWithUid uid = affjax $ defaultRequest
    { method = DELETE
    , url = "/user/" <> encodeURIComponent uid <> ""
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json"]
    }

getUserByUidNameWithUid :: forall eff. String -> Affjax eff Foreign
getUserByUidNameWithUid uid = affjax $ defaultRequest
    { method = GET
    , url = "/user/" <> encodeURIComponent uid <> "/name"
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json"]
    }

getUserByUidEmailWithUid :: forall eff. String -> Affjax eff Foreign
getUserByUidEmailWithUid uid = affjax $ defaultRequest
    { method = GET
    , url = "/user/" <> encodeURIComponent uid <> "/email"
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json"]
    }

postService :: forall eff. String -> Affjax eff Foreign
postService body = affjax $ defaultRequest
    { method = POST
    , url = "/service"
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json"]
    , content = Just body
    }

deleteServiceBySidWithSid :: forall eff. String -> Affjax eff Foreign
deleteServiceBySidWithSid sid = affjax $ defaultRequest
    { method = DELETE
    , url = "/service/" <> encodeURIComponent sid <> ""
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json"]
    }

getService :: forall eff. Affjax eff Foreign
getService = affjax $ defaultRequest
    { method = GET
    , url = "/service"
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json"]
    }

postThentos_session :: forall eff. String -> Affjax eff Foreign
postThentos_session body = affjax $ defaultRequest
    { method = POST
    , url = "/thentos_session"
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json"]
    , content = Just body
    }

getThentos_session :: forall eff. String -> Affjax eff Foreign
getThentos_session body = affjax $ defaultRequest
    { method = GET
    , url = "/thentos_session"
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json"]
    , content = Just body
    }

deleteThentos_session :: forall eff. String -> Affjax eff Foreign
deleteThentos_session body = affjax $ defaultRequest
    { method = DELETE
    , url = "/thentos_session"
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json"]
    , content = Just body
    }

getService_session :: forall eff. String -> Affjax eff Foreign
getService_session body = affjax $ defaultRequest
    { method = GET
    , url = "/service_session"
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json"]
    , content = Just body
    }

getService_sessionMeta :: forall eff. String -> Affjax eff Foreign
getService_sessionMeta body = affjax $ defaultRequest
    { method = GET
    , url = "/service_session/meta"
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json"]
    , content = Just body
    }

deleteService_session :: forall eff. String -> Affjax eff Foreign
deleteService_session body = affjax $ defaultRequest
    { method = DELETE
    , url = "/service_session"
    , headers = [RequestHeader "content-type" "application/json", RequestHeader "accept" "application/json"]
    , content = Just body
    }

