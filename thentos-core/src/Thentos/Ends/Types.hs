{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
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
import Servant.API (Accept (..), MimeRender (..), Post)
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html (Html, ToMarkup, toHtml)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)

import qualified Servant.Foreign as Foreign

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

-- | FIXME: Foreign.Elem is only exported since https://github.com/haskell-servant/servant/pull/265
-- which we don't have, so instead of:
--
-- >>> instance Elem JSON cts => HasForeign (Post200 cts a) where ...
-- >>> instance Elem PNG cts => HasForeign (Post200 cts a) where ...
--
-- we more / less restrictive instances.  We should merge servant master in our submodule branch,
-- though.
instance {-# OVERLAPPING #-} Foreign.HasForeign Foreign.NoTypes (Post '[PNG] a) where
    type Foreign (Post '[PNG] a) = Foreign.Req
    foreignFor Proxy Proxy req =
        req & Foreign.funcName  %~ ("post" :)
            & Foreign.reqMethod .~ "POST"

instance {-# OVERLAPPING #-} Foreign.HasForeign Foreign.NoTypes (Post '[WAV] a) where
    type Foreign (Post '[WAV] a) = Foreign.Req
    foreignFor Proxy Proxy req =
        req & Foreign.funcName  %~ ("post" :)
            & Foreign.reqMethod .~ "POST"
