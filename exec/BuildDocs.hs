{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE OverlappingInstances                     #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE LambdaCase                               #-}
{-# LANGUAGE TypeOperators                            #-}

{-# OPTIONS -fno-warn-orphans #-}

module Main where

import Control.Applicative (pure, (<*>))
import Data.Char (toLower)
import Data.Functor.Infix ((<$>))
import Data.Proxy (Proxy(Proxy))
import Data.Thyme (fromSeconds)
import Data.Thyme.Time ()
import Servant.API (Capture, (:>))
import Servant.Docs
    ( ToCapture(..), DocCapture(DocCapture), ToSample(toSample), HasDocs, docsFor, docs )
import Servant.Docs.Pandoc (pandoc)
import System.Directory (setCurrentDirectory)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((<.>))
import System.Process (system)
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import Text.Pandoc (writeMarkdown, writeHtml, writeDocx, def)

import qualified Data.ByteString.Lazy as LBS
import qualified Servant.Docs as Docs

import Thentos.Backend.Core
import Thentos.Backend.Api.Auth
import Thentos.Types

import qualified Thentos.Backend.Api.Adhocracy3 as Adhocracy3
import qualified Thentos.Backend.Api.Simple as Simple


targetPath :: FilePath
targetPath = "./docs/generated/"

main :: IO ()
main = do
    xsystem $ "mkdir -p " ++ targetPath
    setCurrentDirectory targetPath
    sequence_ $ xwriter <$> [minBound..] <*> [minBound..]

xwriter :: ApiName -> FormatName -> IO ()
xwriter apiName formatName = do
    let fileName = map toLower $ show apiName
        doc = pandoc $ xdocs apiName
    case formatName of
        Format_Markdown
            -> writeFile (fileName <.> "md") (writeMarkdown def doc)
        Format_Html
            -> LBS.writeFile (fileName <.> "html") (renderMarkup $ writeHtml def doc)
        Format_Docx
            -> writeDocx def doc >>= LBS.writeFile (fileName <.> "docx")

-- FIXME: this crashes for poorly understood reasons:
--
--        Format_Pdf
--            -> do
--                 makePDF "pdflatex" writeLaTeX def doc >>=
--                     \case (Right pdf) -> LBS.writeFile (fileName <.> "pdf") pdf
--                           (Left bad) -> error $ show bad

xdocs :: ApiName -> Docs.API
xdocs Api_Simple     = docs (Proxy :: Proxy Simple.Api)
xdocs Api_Adhocracy3 = docs (Proxy :: Proxy Adhocracy3.Api)

data ApiName = Api_Simple | Api_Adhocracy3
  deriving (Eq, Enum, Bounded, Read, Show)

data FormatName = Format_Markdown | Format_Html | Format_Docx
  deriving (Eq, Enum, Bounded, Read, Show)


xsystem :: String -> IO ()
xsystem cmd = do
    putStrLn $ ">> " ++ cmd
    ExitSuccess <- system cmd
    return ()


instance HasDocs sublayout => HasDocs (ThentosAssertHeaders :> sublayout) where
    docsFor Proxy = docsFor (Proxy :: Proxy sublayout)

instance HasDocs sublayout => HasDocs (ThentosAuth :> sublayout) where
    docsFor Proxy = docsFor (Proxy :: Proxy sublayout)


-- | (can this be derived entirely?)
instance ToSample Adhocracy3.A3UserNoPass Adhocracy3.A3UserNoPass where
    toSample _ = Adhocracy3.A3UserNoPass <$> toSample (Proxy :: Proxy UserFormData)

instance ToSample Adhocracy3.A3UserWithPass Adhocracy3.A3UserWithPass where
    toSample _ = Adhocracy3.A3UserWithPass <$> toSample (Proxy :: Proxy UserFormData)

instance ToSample a a => ToSample (Adhocracy3.A3Resource a) (Adhocracy3.A3Resource a) where
    toSample _ = Adhocracy3.A3Resource
                    <$> (Just <$> toSample (Proxy :: Proxy Adhocracy3.Path))
                    <*> (Just <$> toSample (Proxy :: Proxy Adhocracy3.ContentType))
                    <*> (Just <$> toSample (Proxy :: Proxy a))

instance ToSample Adhocracy3.Path Adhocracy3.Path where
    toSample _ = pure $ Adhocracy3.Path "/proposals/environment"

instance ToSample Adhocracy3.ActivationRequest Adhocracy3.ActivationRequest where
    toSample _ = Adhocracy3.ActivationRequest <$> toSample (Proxy :: Proxy Adhocracy3.Path)

instance ToSample Adhocracy3.LoginRequest Adhocracy3.LoginRequest where
    toSample _ = pure $ Adhocracy3.LoginByName (UserName "wef") (UserPass "passwef")

instance ToSample Adhocracy3.RequestResult Adhocracy3.RequestResult where
    toSample _ = Adhocracy3.RequestSuccess
                    <$> toSample (Proxy :: Proxy Adhocracy3.Path)
                    <*> toSample (Proxy :: Proxy ThentosSessionToken)

instance ToSample Adhocracy3.ContentType Adhocracy3.ContentType where
    toSample _ = pure $ Adhocracy3.CTUser

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

instance ToSample UserFormData UserFormData where
    toSample _ = Just $ UserFormData (UserName "Kurt Cobain")
                                   (UserPass "Hunter2")
                                   (UserEmail "cobain@nirvana.com")

instance ToSample UserName UserName where
    toSample _ = Just $ UserName "Kurt Cobain"

instance ToSample UserEmail UserEmail where
    toSample _ = Just $ UserEmail "cobain@nirvana.com"

instance ToSample UserId UserId where
    toSample _ = Just $ UserId 12

instance ToSample (UserId, UserPass) (UserId, UserPass) where
    toSample _ = Just (UserId 12, UserPass "geheim")

instance ToSample [UserId] [UserId] where
    toSample _ = Just [UserId 3, UserId 7, UserId 23]

instance ToSample ServiceId ServiceId where
    toSample _ = Just "23t92ege0n"

instance ToSample ServiceKey ServiceKey where
    toSample _ = Just "yd090129rj"

instance ToSample ServiceName ServiceName where
    toSample _ = Just "Evil Corp."

instance ToSample ServiceDescription ServiceDescription where
    toSample _ = Just "don't be evil"

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

-- | cover for tuples whose components have already been given
-- examples.  if you write an instance for a tuple for two concrete
-- types, `-XOverlappingInstances` will disregard this instance as
-- more general.
instance (ToSample a a, ToSample b b) => ToSample (a, b) (a, b) where
    toSample _ = (,) <$> toSample (Proxy :: Proxy a) <*> toSample (Proxy :: Proxy b)

instance (ToSample a a, ToSample b b, ToSample c c) => ToSample (a, b, c) (a, b, c) where
    toSample _ = (,,) <$> toSample (Proxy :: Proxy a) <*> toSample (Proxy :: Proxy b) <*> toSample (Proxy :: Proxy c)
