{-# LANGUAGE ConstraintKinds                          #-}
{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TypeOperators                            #-}
{-# LANGUAGE UndecidableInstances                     #-}

{-# OPTIONS -fno-warn-orphans #-}

module Thentos.Backend.Api.Docs.Common
    ( RestDocs
    , HasDocExtras(getCabalPackageName, getCabalPackageVersion, getTitle, getIntros, getExtraInfo)
    , HasFullDocExtras
    , restDocs
    , restDocsMd
    , restDocsJs
    , restDocsNg
    , hackTogetherSomeReasonableOrder
    )
where

import Data.Version (Version, showVersion)
import Servant.API (Capture, (:>), Get, (:<|>)((:<|>)), MimeRender(mimeRender))
import Servant.API.Capture ()
import Servant.API.ContentTypes (PlainText)
import Servant.Docs.Internal.Pretty (Pretty)
import Servant.Docs (ToCapture(..), DocCapture(DocCapture), ToSample(toSamples), HasDocs,
                     docsFor, pretty, emptyAPI)
import Servant.Server (ServerT)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Text as ST
import qualified Servant.Docs as Docs
import qualified Servant.Docs.Internal as Docs
import qualified Servant.Foreign as F
import qualified Servant.JS as JS

import Thentos.Prelude
import Thentos.Backend.Api.Auth
import Thentos.Backend.Core
import Thentos.Config
import Thentos.Types

import qualified Thentos.Action as Action
import qualified Thentos.Action.Core as Action
import qualified Thentos.Action.Types as Action


-- * docs via http

-- FIXME: move MimeRender, ToSample API instances and HasDocExtras class upstream to servant-docs

type RestDocs api = RestDocs' api :<|> api
type RestDocs' api = "docs" :>
      ("md"   :> Get '[PlainText] Docs.API
  :<|> "js"   :> Get '[PlainText] ST
  :<|> "ng"   :> Get '[PlainText] ST)

instance MimeRender PlainText Docs.API where
    mimeRender _ = mimeRender (Proxy :: Proxy PlainText) . Docs.markdown

instance ToSample Docs.API where
    toSamples _ = [("empty", emptyAPI)]

instance ToSample ST where
    toSamples _ = [("empty", "")]

-- | the 'Raw' endpoint has 'Foreign', but it is @Method -> Req@, which doesn't have a
-- 'GenerateList' instance.  so, there.  you got one, type checker.  and since @F.Method@ is
-- not exported, we keep it polymorphic.
instance {-# OVERLAPPABLE #-} F.GenerateList ftype (a -> F.Req ftype) where
    generateList _ = []

class HasDocs api => HasDocExtras api where
    getCabalPackageName :: Proxy api -> ST  -- ^ the name of the source package delivering the api
    getCabalPackageVersion :: Proxy api -> Version  -- ^ the package version

    getTitle :: Proxy api -> String

    getIntros :: Proxy api -> [Docs.DocIntro]
    getIntros _ = mempty

    getExtraInfo :: Proxy api -> Docs.ExtraInfo api
    getExtraInfo _ = mempty

type HasFullDocExtras api =
    ( HasDocs (RestDocs api), HasDocs (Pretty api), HasDocExtras (RestDocs api)
    , F.HasForeign F.NoTypes () api, F.GenerateList () (F.Foreign () api)
    )

restDocs :: forall api m. (Monad m, HasFullDocExtras api)
         => HttpConfig -> Proxy (RestDocs api) -> ServerT (RestDocs' api) m
restDocs _ proxy =
        pure (restDocsMd proxy)
   :<|> pure (restDocsJs proxy)
   :<|> pure (restDocsNg proxy)


restDocsMd :: forall api. (HasDocs (Pretty api), HasDocExtras (RestDocs api))
    => Proxy (RestDocs api) -> Docs.API
restDocsMd proxy = hackTogetherSomeReasonableOrder $
        Docs.docsWith (Docs.DocOptions 2)
            intros unsafeCoerceGetExtraInfo (pretty (Proxy :: Proxy api))
      where
        intros :: [Docs.DocIntro]
        intros = Docs.DocIntro ("@@0.0@@" ++ getTitle proxy)
                   [ ("package: " <> cs (getCabalPackageName proxy)) <>
                     (case showVersion (getCabalPackageVersion proxy) of
                         "" -> " (no version info)"
                         v -> " (version: " <> v <> ")") ]
               : getIntros proxy

        unsafeCoerceGetExtraInfo :: forall api'. Docs.ExtraInfo api'
        unsafeCoerceGetExtraInfo = case getExtraInfo proxy of Docs.ExtraInfo m -> Docs.ExtraInfo m

restDocsJs :: forall api. HasFullDocExtras api => Proxy (RestDocs api) -> ST
restDocsJs proxy = restDocsSource proxy "// "
    <> JS.jsForAPI (Proxy :: Proxy api) JS.vanillaJS

restDocsNg :: forall api. HasFullDocExtras api => Proxy (RestDocs api) -> ST
restDocsNg proxy = restDocsSource proxy "// "
    <> JS.jsForAPI (Proxy :: Proxy api) (JS.angular JS.defAngularOptions)

restDocsSource :: HasDocExtras (RestDocs api) => Proxy (RestDocs api) -> ST -> ST
restDocsSource proxy comment = ST.unlines . (ST.stripEnd . (comment <>) <$>) $
        "" :
        "DO NOT EDIT!  THIS IS GENERATED REST API CLIENT CODE!" :
        "" :
        "source package: " <> getCabalPackageName proxy :
        "source package version: " <> (cs . show . getCabalPackageVersion $ proxy) :
        "" :
        []


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
hackTogetherSomeReasonableOrder (Docs.API intros endpoints) = Docs.API (f <$> sort intros) endpoints
  where
    f di@(Docs.DocIntro title desc) = Docs.DocIntro (g title) desc
      where
        g ('@':'@':x) = h $ dropWhile (`elem` (".0123456789" :: String)) x
        g _ = error $ "hackTogetherSomeReasonableOrder/g: " ++ show di

        h ('@':'@':x) = x
        h _ = error $ "hackTogetherSomeReasonableOrder/h: " ++ show di


-- * generating sample tokens

runTokenBuilder :: Action.ActionStack Void () a -> [(ST, a)]
runTokenBuilder action = unsafePerformIO $ Docs.singleSample <$> do
    fst <$> Action.runActionWithClearance dcTop () runTokenBuilderState action

{-# NOINLINE runTokenBuilderState #-}
runTokenBuilderState :: Action.ActionEnv
runTokenBuilderState = unsafePerformIO $ do
    conn <- pure $ error "runTokenBuilder: no db"
    cfg  <- pure $ error "runTokenBuilder: no config"
    return $ Action.ActionEnv cfg conn


-- * instances for servant-docs

instance ToCapture (Capture "token" ThentosSessionToken) where
    toCapture _ = DocCapture "token" "session token for session with thentos"

instance ToCapture (Capture "token" ServiceSessionToken) where
    toCapture _ = DocCapture "token" "session token for thentos-managed session with service"

instance ToCapture (Capture "sid" ServiceId) where
    toCapture _ = DocCapture "sid" "service ID"

instance ToCapture (Capture "uid" UserId) where
    toCapture _ = DocCapture "uid" "user ID"

instance ToCapture (Capture "voice" ST) where
    toCapture _ = DocCapture "voice" "voice file for espeak(1).  run `espeak --voices` for a list."

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

instance ToSample WrappedEmail where
    toSamples _ = second WrappedEmail <$> toSamples (Proxy :: Proxy UserEmail)

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

instance ToSample PasswordResetRequest where
    toSamples _ = second (uncurry PasswordResetRequest)
                    <$> toSamples (Proxy :: Proxy (PasswordResetToken, UserPass))

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

instance ToSample Uri where
    toSamples _ = Docs.singleSample $ parseUri "https://example.com/some-service/user/41" ^?! _Right

instance ToSample PersonaId where
    toSamples _ = Docs.singleSample $ PersonaId 32

instance ToSample EmailRecipients

instance ToSample SendEmailRequest

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

instance ToSample SBS where
    toSamples _ = [("empty", "")]
