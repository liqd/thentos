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
import Data.Aeson.Encode.Pretty (encodePretty', defConfig, Config(confCompare))
import Data.Aeson.Utils (decodeV)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Map (Map)
import Data.String.Conversions (LBS)
import Data.Proxy (Proxy(Proxy))
import Data.Thyme (fromSeconds)
import Network.HTTP.Media (MediaType)
import Safe (fromJustNote)
import Servant.API (Capture, (:>))
import Servant.Docs
    ( ToCapture(..), DocCapture(DocCapture), ToSample(toSample), HasDocs, docsFor)

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

instance ToSample Agent Agent where
    toSample _ = Just . UserA . UserId $ 0

instance ToSample ThentosSessionToken ThentosSessionToken where
    toSample _ = Just "abde1234llkjh"

instance ToSample [ThentosSessionToken] [ThentosSessionToken] where
    toSample _ = Just ["abde1234llkjh", "47202sdfsg"]

-- FIXME: long request bodys should be pretty-printed
instance ToSample UserFormData UserFormData where
    toSample _ = UserFormData <$> toSample (Proxy :: Proxy UserName)
                              <*> toSample (Proxy :: Proxy UserPass)
                              <*> toSample (Proxy :: Proxy UserEmail)

instance ToSample UserPass UserPass where
    toSample _ = Just $ UserPass "secret"

instance ToSample UserName UserName where
    toSample _ = Just $ UserName "Alice"

instance ToSample UserEmail UserEmail where
    toSample _ = Just $ fromMaybe (error "ToSample UserEmail instance broken")
                                  (parseUserEmail "alice@example.com")

instance ToSample UserId UserId where
    toSample _ = Just $ UserId 12

instance ToSample (UserId, UserPass) (UserId, UserPass) where
    toSample _ = (,) <$> toSample (Proxy :: Proxy UserId)
                     <*> toSample (Proxy :: Proxy UserPass)

instance ToSample [UserId] [UserId] where
    toSample _ = Just [UserId 3, UserId 7, UserId 23]

instance ToSample ServiceId ServiceId where
    toSample _ = Just "23t92ege0n"

instance ToSample ServiceKey ServiceKey where
    toSample _ = Just "yd090129rj"

instance ToSample ServiceName ServiceName where
    toSample _ = Just "Example Service"

instance ToSample ServiceDescription ServiceDescription where
    toSample _ = Just "serve as an example"

instance ToSample [ServiceId] [ServiceId] where
    toSample _ = Just ["23t92ege0n", "f4ghwgegin0"]

instance ToSample (UserId, Timeout) (UserId, Timeout) where
    toSample _ = (,) <$> toSample (Proxy :: Proxy UserId) <*> pure (Timeout $ fromSeconds (123456.0 :: Double))

instance ToSample (UserId, ServiceId) (UserId, ServiceId) where
    toSample _ = (,) <$> toSample (Proxy :: Proxy UserId) <*> toSample (Proxy :: Proxy ServiceId)

instance ToSample ServiceSessionMetadata ServiceSessionMetadata where
    toSample _ = ServiceSessionMetadata <$> toSample (Proxy :: Proxy UserName)

instance ToSample ServiceSessionToken ServiceSessionToken where
    toSample _ = Just $ ServiceSessionToken "abde1234llkjh"

instance ToSample () () where
    toSample _ = Just ()

instance ToSample Bool Bool where
    toSample _ = Just True

instance ToSample ByUserOrServiceId ByUserOrServiceId where
    toSamples _ = catMaybes [ (,) "user" . ByUser <$> toSample p1
                            , (,) "service" .  ByService <$> toSample p2]
      where p1 = Proxy :: Proxy (UserId, UserPass)
            p2 = Proxy :: Proxy (ServiceId, ServiceKey)


instance (ToSample a a', ToSample b b') => ToSample (Either a b) (Either a' b') where
    toSamples _ = catMaybes [(,) "Left" . Left <$> toSample p1, (,) "Right" . Right <$> toSample p2]
      where p1 = Proxy :: Proxy a
            p2 = Proxy :: Proxy b


-- | cover for tuples whose components have already been given
-- examples.  if you write an instance for a tuple for two concrete
-- types, this instance will be disregarded because it is more general.
instance {-# OVERLAPPABLE #-} (ToSample a a, ToSample b b) => ToSample (a, b) (a, b) where
    toSample _ = (,) <$> toSample (Proxy :: Proxy a) <*> toSample (Proxy :: Proxy b)

instance {-# OVERLAPPABLE #-} (ToSample a a, ToSample b b, ToSample c c) => ToSample (a, b, c) (a, b, c) where
    toSample _ = (,,) <$> toSample (Proxy :: Proxy a) <*> toSample (Proxy :: Proxy b) <*> toSample (Proxy :: Proxy c)

instance HasDocs sublayout => HasDocs (ThentosAuth :> sublayout) where
    docsFor _ dat = docsFor (Proxy :: Proxy sublayout) dat & Docs.apiIntros %~ (intros ++)
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
    docsFor _ dat = docsFor (Proxy :: Proxy sublayout) dat & Docs.apiIntros %~ (intros ++)
      where
        intros = [Docs.DocIntro title [text]]
        text = "If a request has a headers starting with \"X-Thentos-\\*\" where\
               \ * is any string except \"Service\" or \"Session\", the request\
               \ will be rejected."
        title = "Request Headers"
