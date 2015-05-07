{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE OverlappingInstances                     #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE LambdaCase                               #-}

{-# OPTIONS -fno-warn-orphans #-}

module Main where

import Control.Applicative (pure, (<*>))
import Data.Char (toLower)
import Data.Functor.Infix ((<$>))
import Data.Proxy (Proxy(Proxy))
import Data.Thyme (fromSeconds)
import Data.Thyme.Time ()
import Servant.API (Capture)
import Servant.Docs (HasDocs, docsFor, docs)
import Servant.Docs.Pandoc (pandoc)
import Servant.Docs (ToCapture(..), DocCapture(DocCapture), ToSample(toSample))
import System.Directory (setCurrentDirectory)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((<.>))
import System.Process (system)
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import Text.Pandoc (writeMarkdown, writeHtml, writeDocx, def)

import qualified Data.ByteString.Lazy as LBS
import qualified Servant.Docs as Docs

import Thentos.Backend.Core
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
xdocs Api_Simple     = docs (Proxy :: Proxy Simple.App)
xdocs Api_Adhocracy3 = docs (Proxy :: Proxy Adhocracy3.App)

data ApiName = Api_Simple | Api_Adhocracy3
  deriving (Eq, Enum, Bounded, Read, Show)

data FormatName = Format_Markdown | Format_Html | Format_Docx
  deriving (Eq, Enum, Bounded, Read, Show)


xsystem :: String -> IO ()
xsystem cmd = do
    putStrLn $ ">> " ++ cmd
    ExitSuccess <- system cmd
    return ()


instance HasDocs sublayout => HasDocs (Simple.ThentosAuth sublayout) where
    docsFor Proxy = docsFor (Proxy :: Proxy sublayout)

instance HasDocs sublayout => HasDocs (ThentosAssertHeaders sublayout) where
    docsFor Proxy = docsFor (Proxy :: Proxy sublayout)


-- | (can this be derived entirely?)
instance ToSample Adhocracy3.A3UserNoPass where
    toSample = Adhocracy3.A3UserNoPass <$> toSample

instance ToSample Adhocracy3.A3UserWithPass where
    toSample = Adhocracy3.A3UserWithPass <$> toSample

instance ToSample a => ToSample (Adhocracy3.A3Resource a) where
    toSample = Adhocracy3.A3Resource <$> (Just <$> toSample) <*> (Just <$> toSample) <*> (Just <$> toSample)

instance ToSample Adhocracy3.Path where
    toSample = pure $ Adhocracy3.Path "/proposals/environment"

instance ToSample Adhocracy3.ActivationRequest where
    toSample = Adhocracy3.ActivationRequest <$> toSample

instance ToSample Adhocracy3.LoginRequest where
    toSample = pure $ Adhocracy3.LoginByName (UserName "wef") (UserPass "passwef")

instance ToSample Adhocracy3.RequestResult where
    toSample = Adhocracy3.RequestSuccess <$> toSample <*> toSample

instance ToSample Adhocracy3.ContentType where
    toSample = pure $ Adhocracy3.CTUser

instance ToCapture (Capture "token" SessionToken) where
    toCapture _ = DocCapture "token" "Session Token"

instance ToCapture (Capture "token" ServiceSessionToken) where
    toCapture _ = DocCapture "token" "Service Session Token"

instance ToCapture (Capture "sid" ServiceId) where
    toCapture _ = DocCapture "sid" "Service ID"

instance ToCapture (Capture "uid" UserId) where
    toCapture _ = DocCapture "uid" "User ID"

instance ToSample Agent where
    toSample = Just . UserA . UserId $ 0

instance ToSample SessionToken where
    toSample = Just "abde1234llkjh"

instance ToSample [SessionToken] where
    toSample = Just ["abde1234llkjh", "47202sdfsg"]

instance ToSample UserFormData where
    toSample = Just $ UserFormData (UserName "Kurt Cobain")
                                   (UserPass "Hunter2")
                                   (UserEmail "cobain@nirvana.com")

instance ToSample UserName where
    toSample = Just $ UserName "Kurt Cobain"

instance ToSample UserEmail where
    toSample = Just $ UserEmail "cobain@nirvana.com"

instance ToSample UserId where
    toSample = Just $ UserId 12

instance ToSample (UserId, UserPass) where
    toSample = Just (UserId 12, UserPass "geheim")

instance ToSample [UserId] where
    toSample = Just [UserId 3, UserId 7, UserId 23]

instance ToSample ServiceId where
    toSample = Just "23t92ege0n"

instance ToSample ServiceKey where
    toSample = Just "yd090129rj"

instance ToSample ServiceName where
    toSample = Just "Evil Corp."

instance ToSample ServiceDescription where
    toSample = Just "don't be evil"

instance ToSample [ServiceId] where
    toSample = Just ["23t92ege0n", "f4ghwgegin0"]

instance ToSample (UserId, Timeout) where
    toSample = (,) <$> toSample <*> pure (Timeout $ fromSeconds (123456.0 :: Double))

instance ToSample (UserId, ServiceId) where
    toSample = (,) <$> toSample <*> toSample

instance ToSample ServiceSessionMetaData where
    toSample = ServiceSessionMetaData <$> toSample

instance ToSample () where
    toSample = Just ()

instance ToSample Bool where
    toSample = Just True

-- | cover for tuples whose components have already been given
-- examples.  if you write an instance for a tuple for two concrete
-- types, `-XOverlappingInstances` will disregard this instance as
-- more general.
instance (ToSample a, ToSample b) => ToSample (a, b) where
    toSample = (,) <$> toSample <*> toSample

instance (ToSample a, ToSample b, ToSample c) => ToSample (a, b, c) where
    toSample = (,,) <$> toSample <*> toSample <*> toSample
