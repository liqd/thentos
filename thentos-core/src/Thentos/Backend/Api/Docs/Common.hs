{-# LANGUAGE AllowAmbiguousTypes                      #-}
{-# LANGUAGE CPP                                      #-}
{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE PackageImports                           #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TypeFamilies                             #-}
{-# LANGUAGE TypeOperators                            #-}
{-# LANGUAGE UndecidableInstances                     #-}

{-# OPTIONS -fno-warn-orphans #-}

module Thentos.Backend.Api.Docs.Common
    ( RestDocs
    , HasDocExtras(getCabalVersion, getTitle, getIntros, getExtraInfo)
    , restDocs
    , prettyMimeRender
    , hackTogetherSomeReasonableOrder
    )
where

import Control.Arrow (second)
import Control.Concurrent.MVar (newMVar)
import Control.Lens ((&), (%~), (.~))
import "cryptonite" Crypto.Random (drgNew)
import Data.Aeson.Encode.Pretty (encodePretty', defConfig, Config(confCompare))
import Data.Aeson.Utils (decodeV)
import Data.List (sort)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (ST, LBS)
import Data.Version (Version)
import Data.Void (Void)
import Network.HTTP.Media (MediaType)
import Safe (fromJustNote)
import Servant.API (Capture, (:>), Post, Get, (:<|>), MimeRender(mimeRender))
import Servant.API.ContentTypes (AllMimeRender, IsNonEmpty, PlainText)
import Servant.Docs (ToCapture(..), DocCapture(DocCapture), ToSample(toSamples), HasDocs,
                     docsFor, emptyAPI)
import Servant.Docs.Internal (API(API), response, respStatus)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Servant.Docs as Docs

import Thentos.Backend.Api.Auth
import Thentos.Backend.Core
import Thentos.Types

import qualified LIO.Missing
import qualified Thentos.Action as Action
import qualified Thentos.Action.Core as Action


-- * docs via http

-- FIXME: move MimeRender, ToSample API instances and HasDocExtras class upstream to servant-docs

type RestDocs api = RestDocs' api :<|> api
type RestDocs' api = "docs" :> "md" :> Get '[PlainText] Docs.API

instance MimeRender PlainText API where
    mimeRender _ = mimeRender (Proxy :: Proxy PlainText) . Docs.markdown

instance ToSample API where
    toSamples _ = [("empty", emptyAPI)]

class HasDocs api => HasDocExtras api where
    getCabalVersion :: Proxy api -> Version
    getTitle :: Proxy api -> String

    getIntros :: Proxy api -> [Docs.DocIntro]
    getIntros _ = mempty

    getExtraInfo :: Proxy api -> Docs.ExtraInfo api
    getExtraInfo _ = mempty

restDocs :: forall api. (HasDocs (RestDocs api), HasDocExtras (RestDocs api))
         => Proxy (RestDocs api) -> API
restDocs proxy = prettyMimeRender . hackTogetherSomeReasonableOrder $
    Docs.docsWith
        (Docs.DocOptions 2)
        (intro : getIntros proxy)
        (getExtraInfo proxy)
        proxy
  where
    intro = Docs.DocIntro ("@@0.0@@" ++ getTitle proxy) [show $ getCabalVersion proxy]

-- | The `servant-docs` package does offer a way to explicitly order intros (I'm not even sure if
-- the implicit order is deterministic).  This function allows you to write intros with titles of
-- the form @\@\@...\@\@@, where @...@ contains section numbers composed of digits and dots.  It
-- will call 'sort' on the list of intros then then chop of the section numbers off the intro
-- titles.  This way, the section numbers determine the order, and even if you don't provide section
-- headings, everything will still work, and the intro list will be deterministic (even though not
-- always meaningful).
--
-- The name of this function suggests that there may be a better way to solve this.  (For one, the
-- section numbers are ordered lexicographically, not numerically: @compare "\@\@0.1\@\@"
-- "\@\@0\@\@" == LT@.)
hackTogetherSomeReasonableOrder :: Docs.API -> Docs.API
hackTogetherSomeReasonableOrder (API intros endpoints) = API (f <$> sort intros) endpoints
  where
    f di@(Docs.DocIntro title desc) = Docs.DocIntro (g title) desc
      where
        g ('@':'@':x) = h $ dropWhile (`elem` (".0123456789" :: String)) x
        g _ = error $ "hackTogetherSomeReasonableOrder/g: " ++ show di

        h ('@':'@':x) = x
        h _ = error $ "hackTogetherSomeReasonableOrder/h: " ++ show di


-- * Pretty-printing

prettyMimeRender' :: Map MediaType (LBS -> LBS) -> Docs.API -> Docs.API
prettyMimeRender' pprinters = Docs.apiEndpoints %~ updateEndpoints
  where
    updateEndpoints = HM.map (pprintAction pprinters)

prettyMimeRender :: Docs.API -> Docs.API
prettyMimeRender = prettyMimeRender' $ Map.fromList [("application/json", pprintJson)]

pprintJson :: LBS -> LBS
pprintJson raw = encodePretty' (defConfig {confCompare = compare})
           . fromJustNote ("Internal error in Thentos.Backend.Api.Docs.Common:" ++
                           " Non-invertible ToJSON instance detected: " ++ show raw)
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


-- * generating sample tokens

runTokenBuilder :: Action.Action Void () a -> [(ST, a)]
runTokenBuilder action = unsafePerformIO $ Docs.singleSample <$> do
    fst <$> Action.runActionWithClearance LIO.Missing.dcTop () runTokenBuilderState action

{-# NOINLINE runTokenBuilderState #-}
runTokenBuilderState :: Action.ActionState
runTokenBuilderState = unsafePerformIO $ do
    rng  <- drgNew >>= newMVar
    conn <- pure $ error "runTokenBuilder: no db"
    cfg  <- pure $ error "runTokenBuilder: no config"
    return $ Action.ActionState (conn, rng, cfg)


-- * instances for servant-docs

instance ToCapture (Capture "token" ThentosSessionToken) where
    toCapture _ = DocCapture "token" "session token for session with thentos"

instance ToCapture (Capture "token" ServiceSessionToken) where
    toCapture _ = DocCapture "token" "session token for thentos-managed session with service"

instance ToCapture (Capture "sid" ServiceId) where
    toCapture _ = DocCapture "sid" "service ID"

instance ToCapture (Capture "uid" UserId) where
    toCapture _ = DocCapture "uid" "user ID"

instance (ToSample a) => ToSample (JsonTop a) where
    toSamples _ = second JsonTop <$> toSamples (Proxy :: Proxy a)

instance ToSample Agent where
    toSamples _ = Docs.singleSample . UserA . UserId $ 0

instance ToSample ThentosSessionToken where
    toSamples _ = runTokenBuilder Action.freshSessionToken

instance ToSample LoginFormData where
    toSamples _ = second (uncurry LoginFormData)
                    <$> toSamples (Proxy :: Proxy (UserName, UserPass))

instance ToSample UserFormData where
    toSamples _ = let uncurry3 f (a, b, c) = f a b c
                  in second (uncurry3 UserFormData)
                    <$> toSamples (Proxy :: Proxy (UserName, UserPass, UserEmail))

instance ToSample UserPass where
    toSamples _ = Docs.singleSample $ UserPass "secret"

instance ToSample UserName where
    toSamples _ = Docs.singleSample $ UserName "alice"

instance ToSample UserEmail where
    toSamples _ = Docs.singleSample . (\(Just e) -> e) $ parseUserEmail "alice@example.com"

instance ToSample UserId where
    toSamples _ = Docs.singleSample $ UserId 12

instance ToSample ImageData where
    toSamples _ = Docs.singleSample $ ImageData "<large blob of unreadable binary gibberish>"

instance ToSample CaptchaId where
    toSamples _ = runTokenBuilder Action.freshCaptchaId

instance ToSample CaptchaSolution where
    toSamples _ = do
      let cid :: CaptchaId = fromJustNote "ToSample CaptchaSolution failed unexpectedly" $
                                          Docs.toSample (Proxy :: Proxy CaptchaId)
      Docs.singleSample $ CaptchaSolution cid "someTeXT"

instance ToSample UserCreationRequest where
    toSamples _ = second (uncurry UserCreationRequest)
                    <$> toSamples (Proxy :: Proxy (UserFormData, CaptchaSolution))

instance ToSample ConfirmationToken where
    toSamples _ = runTokenBuilder Action.freshConfirmationToken

instance ToSample PasswordResetToken where
    toSamples _ = runTokenBuilder Action.freshPasswordResetToken

instance ToSample ServiceId where
    toSamples _ = runTokenBuilder Action.freshServiceId

instance ToSample ServiceKey where
    toSamples _ = runTokenBuilder Action.freshServiceKey

instance ToSample ServiceName where
    toSamples _ = Docs.singleSample "Evil Corp."

instance ToSample ServiceDescription where
    toSamples _ = Docs.singleSample "Making the worse a little better every day."

instance ToSample ServiceSessionMetadata where
    toSamples _ = second ServiceSessionMetadata <$> toSamples (Proxy :: Proxy UserName)

instance ToSample ServiceSessionToken where
    toSamples _ = runTokenBuilder Action.freshServiceSessionToken

instance ToSample ByUserOrServiceId


instance HasDocs sublayout => HasDocs (ThentosAuth :> sublayout) where
    docsFor _ dat opts = docsFor (Proxy :: Proxy sublayout) dat opts & Docs.apiIntros %~ (intro:)
      where
        intro = Docs.DocIntro "@@1.2@@Authentication" [unlines desc]
        desc = [ "To call any of this API's endpoints as a User or Service,"
               , "your request has to contain an HTTP header with the name"
               , "'X-Thentos-Session' and with the value set to a valid session"
               , "token."
               ]


instance HasDocs sublayout => HasDocs (ThentosAssertHeaders :> sublayout) where
    docsFor _ dat opts = docsFor (Proxy :: Proxy sublayout) dat opts & Docs.apiIntros %~ (intro:)
      where
        intro = Docs.DocIntro "@@1.1@@Request Headers" [unlines desc]
        desc = ["If a request has an unknown header with prefix \"X-Thentos-\"."]


instance {-# OVERLAPPABLE #-} (ToSample a, IsNonEmpty cts, AllMimeRender cts a)
      => HasDocs (Post200 cts a) where
    docsFor Proxy (endpoint, action) opts =
        case docsFor (Proxy :: Proxy (Post cts a)) (endpoint, action) opts of
            API intros singleton -> API intros $ mutate <$> singleton
      where
        mutate = (& response . respStatus .~ 200)
