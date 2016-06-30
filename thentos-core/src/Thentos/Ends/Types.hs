{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

-- | Types required by both backend and frontend.
module Thentos.Ends.Types
    ( HTM
    , renderHTM
    , PrettyHTML
    , TextCss
    , PNG
    , WAV
    )
where

import Control.Lens ((&), (%~), (.~))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (ST, LT, SBS, LBS, cs)
import Network.HTTP.Media ((//), (/:))
import Servant.API (Accept (..), MimeRender (..)) -- , Post)
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html (Html, ToMarkup, toHtml)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)

import qualified Servant.Foreign as F
import qualified Servant.Foreign.Internal as F

import Thentos.Types


-- * content types

-- FUTUREWORK: we will need more of http://www.iana.org/assignments/media-types/media-types.xhtml,
-- and we should probably add all of them either to the servant package or to a new package
-- servant-content-types rather than here.

-- | Html content type with pretty printing.  (See also: package servant-blaze.)
type HTM = PrettyHTML

renderHTM :: Html -> LBS
renderHTM = cs . renderHtml

data PrettyHTML

instance Accept PrettyHTML where
    contentType _ = contentType (Proxy :: Proxy HTML)

instance {-# OVERLAPPABLE #-} ToMarkup a => MimeRender PrettyHTML a where
    mimeRender _ = renderHTM . toHtml

instance {-# OVERLAPPING #-} MimeRender PrettyHTML Html where
    mimeRender _ = renderHTM


data TextCss

instance Accept TextCss where
    contentType _ = "text" // "css" /: ("charset", "utf-8")

instance MimeRender TextCss LBS    where mimeRender _ = id
instance MimeRender TextCss SBS    where mimeRender _ = cs
instance MimeRender TextCss ST     where mimeRender _ = cs
instance MimeRender TextCss LT     where mimeRender _ = cs
instance MimeRender TextCss String where mimeRender _ = cs


data PNG

instance Accept PNG where
    contentType _ = "image" // "png"

instance MimeRender PNG ImageData where
    mimeRender _ = cs . fromImageData


data WAV

instance Accept WAV where
    contentType _ = "audio" // "l16"

instance MimeRender WAV SBS where
    mimeRender _ = cs


-- * servant foreign

-- See https://github.com/haskell-servant/servant/issues/509
-- See https://github.com/haskell-servant/servant/issues/290
{-
More generic instances for PNG and WAV would be better but this fails with the following
error in Captcha.hs: `No instance for Foreign.NotFound arising from a use of `restDocs'`.

instance {-# OVERLAPPING #-} (F.Elem PNG list, F.HasForeignType lang ftype a, F.ReflectMethod method)
  => F.HasForeign lang ftype (F.Verb method status list a) where
  type Foreign ftype (F.Verb method status list a) = F.Req ftype

  foreignFor lang Proxy Proxy req =
    req & F.reqFuncName . F._FunctionName %~ (methodLC :)
        & F.reqMethod .~ method
        & F.reqReturnType .~ Just retType
    where
      retType  = F.typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy a)
      method   = F.reflectMethod (Proxy :: Proxy method)
      methodLC = ST.toLower $ cs method
-}
instance {-# OVERLAPPING #-} F.HasForeignType lang ftype a
  => F.HasForeign lang ftype (F.Verb 'F.POST status '[PNG] a) where
  type Foreign ftype (F.Verb 'F.POST status '[PNG] a) = F.Req ftype

  foreignFor lang Proxy Proxy req =
    req & F.reqFuncName . F._FunctionName %~ ("post" :)
        & F.reqMethod .~ "POST"
        & F.reqReturnType .~ Just retType
    where
      retType  = F.typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy a)

instance {-# OVERLAPPING #-} F.HasForeignType lang ftype a
  => F.HasForeign lang ftype (F.Verb 'F.POST status '[WAV] a) where
  type Foreign ftype (F.Verb 'F.POST status '[WAV] a) = F.Req ftype

  foreignFor lang Proxy Proxy req =
    req & F.reqFuncName . F._FunctionName %~ ("post" :)
        & F.reqMethod .~ "POST"
        & F.reqReturnType .~ Just retType
    where
      retType  = F.typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy a)
