{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PackageImports         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

module Thentos.Frontend.Types where

import Control.Lens (makeLenses)
import Control.Monad (mzero)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Builder (toLazyByteString)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (ST, LT, SBS, LBS, cs)
import GHC.Generics (Generic)
import Servant.API (Accept (..), MimeRender (..))
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html (Html, ToMarkup, toHtml)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import URI.ByteString (RelativeRef, serializeRelativeRef, parseRelativeRef, laxURIParserOptions)

import qualified Data.Aeson as Aeson
import qualified Generics.Generic.Aeson as Aeson
import qualified Network.HTTP.Media as Media

import Thentos.Types


-- * content types

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
    contentType _ = "text" Media.// "css" Media./: ("charset", "utf-8")

instance MimeRender TextCss LBS    where mimeRender _ = id
instance MimeRender TextCss SBS    where mimeRender _ = cs
instance MimeRender TextCss ST     where mimeRender _ = cs
instance MimeRender TextCss LT     where mimeRender _ = cs
instance MimeRender TextCss String where mimeRender _ = cs


-- * session state

data FrontendSessionData =
    FrontendSessionData
        { _fsdLogin             :: Maybe FrontendSessionLoginData
        , _fsdServiceLoginState :: Maybe ServiceLoginState  -- ^ (see 'ServiceLoginState')
        , _fsdMessages          :: [FrontendMsg]
        }
  deriving (Show, Eq, Generic)

emptyFrontendSessionData :: FrontendSessionData
emptyFrontendSessionData = FrontendSessionData Nothing Nothing []

data FrontendSessionLoginData =
    FrontendSessionLoginData
        { _fslToken  :: ThentosSessionToken
        , _fslUserId :: UserId
        }
  deriving (Show, Eq, Generic)

instance FromJSON FrontendSessionData where parseJSON = Aeson.gparseJson
instance ToJSON FrontendSessionData where toJSON = Aeson.gtoJson

instance FromJSON FrontendSessionLoginData where parseJSON = Aeson.gparseJson
instance ToJSON FrontendSessionLoginData where toJSON = Aeson.gtoJson

-- | If a user comes from a service login and is sent to the "register
-- with a new service" page because no valid 'ServiceAccount' exists
-- in thentos, an extra round of indirection is required (the user is
-- confronted with the service details and must say "yes, i want to
-- register", "share this-and-that data", etc.).  There may be further
-- extra rounds for logging in to thentos or registering with thentos.
-- This type is used to store the information from the service login
-- request in 'FrontendSessionData' until all these steps have been
-- taken.
data ServiceLoginState =
    ServiceLoginState
        { _fslServiceId :: ServiceId
        , _fslRR        :: RelativeRef  -- ^ e.g. @/service/login?...@
        }
  deriving (Show, Eq, Generic)

instance ToJSON ServiceLoginState where
    toJSON (ServiceLoginState mSid rr) = Aeson.toJSON (mSid, rr')
      where rr' = Aeson.String . cs . toLazyByteString . serializeRelativeRef $ rr

instance FromJSON ServiceLoginState where
    parseJSON v = do
        (mSid, rr' :: Aeson.Value) <- Aeson.parseJSON v
        case rr' of
            Aeson.String rr'' -> case parseRelativeRef laxURIParserOptions $ cs rr'' of
                (Right rr) -> return $ ServiceLoginState mSid rr
                _ -> mzero
            _ -> mzero

-- | If we want to communicate information from the last request to
-- the user in the next one, we need to stash it in the state.  This
-- is the type for that.
data FrontendMsg =
    FrontendMsgError ST
  | FrontendMsgSuccess ST
  deriving (Show, Eq, Generic)

instance FromJSON FrontendMsg where parseJSON = Aeson.gparseJson
instance ToJSON FrontendMsg where toJSON = Aeson.gtoJson


makeLenses ''FrontendSessionData
makeLenses ''FrontendSessionLoginData
makeLenses ''ServiceLoginState
