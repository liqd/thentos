{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}

{-# OPTIONS -fno-warn-orphans #-}

module Main where

import Control.Applicative (pure, (<*>))
import Data.Functor.Infix ((<$>))
import Data.Proxy (Proxy(Proxy))
import Data.Thyme (fromSeconds)
import Data.Thyme.Time ()
import Servant.API (Capture)
import Servant.Docs (HasDocs, docsFor, docs, markdown)
import Servant.Docs (ToCapture(..), DocCapture(DocCapture), ToSample(toSample))
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>), (<.>))
import System.Process (system)
import System.Directory (setCurrentDirectory)

import Thentos.Types

import qualified Thentos.Backend.Api.Adhocracy3 as Adhocracy3
import qualified Thentos.Backend.Api.Simple as Simple


main :: IO ()
main = do
    let targetPath = "./docs/generated/"
    xsystem $ "mkdir -p " ++ targetPath
    setCurrentDirectory targetPath
    xbuild "simple" (Proxy :: Proxy Simple.App)
    xbuild "adhocracy3" (Proxy :: Proxy Adhocracy3.App)

xbuild :: HasDocs a => String -> Proxy a -> IO ()
xbuild fileName proxy = do
    writeFile (fileName <.> "md") (markdown $ docs proxy)
    xsystem $ "pandoc " ++ fileName <.> "md" ++ " -o " ++ fileName <.> "html"

xsystem :: String -> IO ()
xsystem cmd = do
    putStrLn $ ">> " ++ cmd
    ExitSuccess <- system cmd
    return ()


instance HasDocs sublayout => HasDocs (Simple.ThentosAuth sublayout) where
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

instance ToCapture (Capture "sid" ServiceId) where
    toCapture _ = DocCapture "sid" "Service ID"

instance ToCapture (Capture "userid" UserId) where
    toCapture _ = DocCapture "userid" "User ID"

instance ToSample Agent where
    toSample = Just . UserA . UserId $ 0

instance ToSample Session where
    toSample =
        Session <$> toSample
                <*> pure (TimeStamp $ read "1986-20-09 00:00:00 UTC")
                <*> pure (TimeStamp $ read "1986-27-09 00:00:00 UTC")
                <*> pure (Timeout 600)

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

instance ToSample [UserId] where
    toSample = Just [UserId 3, UserId 7, UserId 23]

instance ToSample ServiceId where
    toSample = Just "23t92ege0n"

instance ToSample [ServiceId] where
    toSample = Just ["23t92ege0n", "f4ghwgegin0"]

instance ToSample (UserId, Timeout) where
    toSample = (,) <$> toSample <*> pure (Timeout $ fromSeconds (123456.0 :: Double))

instance ToSample (UserId, ServiceId) where
    toSample = (,) <$> toSample <*> toSample

instance ToSample () where
    toSample = Just ()

instance ToSample Bool where
    toSample = Just True

instance ToSample (ServiceId, ServiceKey) where
    toSample = Nothing

instance ToSample (SessionToken, Session) where
    toSample = Nothing
