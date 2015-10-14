{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE TypeOperators                            #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE UndecidableInstances                     #-}

{-# OPTIONS -fno-warn-orphans #-}

module Thentos.Backend.Api.Docs.Common (prettyMimeRender) where

import Control.Lens ((&), (%~))
import Control.Arrow (second)
import Data.Aeson.Encode.Pretty (encodePretty', defConfig, Config(confCompare))
import Data.Aeson.Utils (decodeV)
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import Data.String.Conversions (LBS)
import Data.Proxy (Proxy(Proxy))
import Network.HTTP.Media (MediaType)
import Safe (fromJustNote)
import Servant.API (Capture, (:>))
import Servant.Docs
    ( ToCapture(..), DocCapture(DocCapture), ToSample(toSamples), HasDocs, docsFor)

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Servant.Docs as Docs

import Thentos.Backend.Api.Auth
import Thentos.Backend.Core
import Thentos.Types


-- * Pretty-printing

prettyMimeRender' :: Map MediaType (LBS -> LBS) -> Docs.API -> Docs.API
prettyMimeRender' pprinters = Docs.apiEndpoints %~ updateEndpoints
  where
    updateEndpoints = HM.map (pprintAction pprinters)

prettyMimeRender :: Docs.API -> Docs.API
prettyMimeRender = prettyMimeRender' $ Map.fromList [("application/json", pprintJson)]

pprintJson :: LBS -> LBS
pprintJson raw = encodePretty' (defConfig {confCompare = compare})
           . fromJustNote ("Internal error in Thentos.Backend.Api.Docs.Common:\
                           \ Non-invertible ToJSON instance detected: " ++ show raw)
           . (decodeV :: LBS -> Maybe Aeson.Value)
           $ raw

pprintAction :: Map MediaType (LBS -> LBS) -> Docs.Action -> Docs.Action
pprintAction pprinters action = (Docs.rqbody %~ updateReqBody) . (Docs.response %~ updateResponse) $ action
  where
    updateReqBody = map pprintData
    updateResponse = Docs.respBody %~ pprintRespBody
    pprintRespBody = map (\(t, m, bs) -> (t, m, snd (pprintData (m, bs))))

    pprintData :: (MediaType, LBS) -> (MediaType, LBS)
    pprintData (mType, bs) = (mType, pprint bs)
      where pprint = fromMaybe id (Map.lookup mType pprinters)


-- * instances for servant-docs

instance ToCapture (Capture "token" ThentosSessionToken) where
    toCapture _ = DocCapture "token" "Thentos Session Token"

instance ToCapture (Capture "token" ServiceSessionToken) where
    toCapture _ = DocCapture "token" "Service Session Token"

instance ToCapture (Capture "sid" ServiceId) where
    toCapture _ = DocCapture "sid" "Service ID"

instance ToCapture (Capture "uid" UserId) where
    toCapture _ = DocCapture "uid" "User ID"

instance ToSample Agent where
    toSamples _ = Docs.singleSample . UserA . UserId $ 0

instance ToSample ThentosSessionToken where
    toSamples _ = Docs.singleSample "abde1234llkjh"

-- FIXME: long request bodys should be pretty-printed
instance ToSample UserFormData where
    toSamples _ = let curry3 f (a, b, c) = f a b c
                  in second (curry3 UserFormData)
                    <$> toSamples (Proxy :: Proxy (UserName, UserPass, UserEmail))

instance ToSample UserPass where
    toSamples _ = Docs.singleSample $ UserPass "secret"

instance ToSample UserName where
    toSamples _ = Docs.singleSample $ UserName "Alice"

instance ToSample UserEmail where
    toSamples _ = Docs.singleSample $ fromMaybe (error "ToSample UserEmail instance broken")
                                  (parseUserEmail "alice@example.com")

instance ToSample UserId where
    toSamples _ = Docs.singleSample $ UserId 12

instance ToSample ServiceId where
    toSamples _ = Docs.singleSample "23t92ege0n"

instance ToSample ServiceKey where
    toSamples _ = Docs.singleSample "yd090129rj"

instance ToSample ServiceName where
    toSamples _ = Docs.singleSample "Example Service"

instance ToSample ServiceDescription where
    toSamples _ = Docs.singleSample "serve as an example"

instance ToSample ServiceSessionMetadata where
    toSamples _ = second ServiceSessionMetadata <$> toSamples (Proxy :: Proxy UserName)

instance ToSample ServiceSessionToken where
    toSamples _ = Docs.singleSample $ ServiceSessionToken "abde1234llkjh"

instance ToSample ByUserOrServiceId


instance HasDocs sublayout => HasDocs (ThentosAuth :> sublayout) where
    docsFor _ dat opts = docsFor (Proxy :: Proxy sublayout) dat opts & Docs.apiIntros %~ (intros ++)
      where
        intros = [Docs.DocIntro title [text]]
        title = "Authentication"
        text = "To call any of this API's endpoints as a User or Service,\
               \ your request has to contain an HTTP header with the name\
               \ 'X-Thentos-Session' and with the value set to a valid session\
               \ token. Session tokens can be acquired by authenticating to\
               \ the /thentos_session endpoint."
        -- FIXME: is there any way to link to the endpoints we're referring to?


instance HasDocs sublayout => HasDocs (ThentosAssertHeaders :> sublayout) where
    docsFor _ dat opts = docsFor (Proxy :: Proxy sublayout) dat opts & Docs.apiIntros %~ (intros ++)
      where
        intros = [Docs.DocIntro title [text]]
        text = "If a request has a headers starting with \"X-Thentos-\\*\" where\
               \ * is any string except \"Service\" or \"Session\", the request\
               \ will be rejected."
        title = "Request Headers"
