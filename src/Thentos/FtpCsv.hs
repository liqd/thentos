{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveFunctor               #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE KindSignatures              #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ViewPatterns                #-}

-- | This is a quick shot at the requirement to import csv files with user data via ftp.  The
-- approach is not directly extensible, but should be robust enough, and straight-forward enough to
-- refactor.
module Thentos.FtpCsv where

import Control.Applicative ((<*>), pure)
import Control.Exception (Exception)
import Control.Lens (makeLenses, Lens', (^.), (.~))
import Data.Csv ((.!))
import Data.Aeson (FromJSON, ToJSON, Value(String), (.=), (.:))
import Control.Monad (mzero, unless)
import Control.Exception (throwIO)
import Data.Data (Typeable)
import Data.Functor.Infix ((<$>))
import Data.Map (Map)
import Data.SafeCopy (SafeCopy, Contained, deriveSafeCopy, base, contain, putCopy, getCopy,
                      safePut, safeGet)
import Data.Set (Set)
import Data.Monoid ((<>))
import Data.String.Conversions (ST, cs, LBS, SBS)
import Data.String (IsString)
import Data.Thyme.Time () -- required for NominalDiffTime's num instance
import Data.Thyme (UTCTime, NominalDiffTime, formatTime, parseTime, toSeconds, fromSeconds)
import Data.Typeable (Proxy(Proxy), typeOf)
import GHC.Generics (Generic)
import LIO.DCLabel (ToCNF, toCNF)
import Safe (readMay)
import Servant.Common.Text (FromText)
import System.Locale (defaultTimeLocale)
import Text.Email.Validate (EmailAddress, emailAddress, toByteString)

import qualified Crypto.Scrypt as Scrypt
import qualified Data.Aeson as Aeson
import qualified Data.Configifier as Cfg
import qualified Data.Csv as Cassava
import qualified Data.Map as Map
import qualified Data.Serialize as Cereal
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import qualified Generics.Generic.Aeson as Aeson
import qualified Network.FTP.Client as FTP
import qualified Network.FTP.Client.Parser as FTP

import URI.ByteString
import Data.Maybe (isJust, fromMaybe)
import Network.Socket (PortNumber(..))

import Thentos.Types


-- * types

data MyUser =
    MyUser
      { _spdUserId              :: Int
      , _spdUserVorname         :: ST
      , _spdUserNachname        :: ST
      , _spdUserAnrede          :: ST
      , _spdUserMail            :: UserEmail
      , _spdUserFunktion        :: MyFunktion
      , _spdUserMitgliedsnummer :: ST
      , _spdUserTelefon         :: TelephoneNumber
      , _spdUserPlz             :: ST
      , _spdUserStadt           :: ST
      , _spdUserStrasse         :: ST
      , _spdUserHausnummer      :: ST
      , _spdUserKandidaturen    :: [MyKandidatur]
      }
    deriving (Eq, Ord, Show, Read, Typeable, Generic)

data MyFunktion =
    MyFunktionMitglied
  | MyFunktionSonstige ST
    deriving (Eq, Ord, Show, Read, Typeable, Generic)

data MyKandidatur =
    MyKandidatur
      { _spdKandidaturId   :: Int
      , _spdKandidaturWahl :: ST
      }
    deriving (Eq, Ord, Show, Read, Typeable, Generic)

newtype TelephoneNumber = TelephoneNumber { fromTelephoneNumber :: ST }
    deriving (Eq, Ord, Show, Read, Typeable, Generic)


-- * lenses, safecopy

makeLenses ''MyUser
makeLenses ''MyKandidatur

$(deriveSafeCopy 0 'base ''MyUser)
$(deriveSafeCopy 0 'base ''MyFunktion)
$(deriveSafeCopy 0 'base ''MyKandidatur)
$(deriveSafeCopy 0 'base ''TelephoneNumber)


-- * aeson

instance Aeson.FromJSON MyUser where
  parseJSON = Aeson.withObject "MyUser" $ \ o ->
      MyUser
        <$> o .: "id"
        <*> o .: "vorname"
        <*> o .: "nachname"
        <*> o .: "anrede"
        <*> o .: "mail"
        <*> o .: "funktion"
        <*> o .: "mitgliedsnummer"
        <*> o .: "telefon"
        <*> o .: "plz"
        <*> o .: "stadt"
        <*> o .: "strasse"
        <*> o .: "hausnummer"
        <*> o .: "kandidaturen"

instance Aeson.ToJSON MyUser where
  toJSON u = Aeson.object
      [ "id"              .= (u ^. spdUserId)
      , "vorname"         .= (u ^. spdUserVorname)
      , "nachname"        .= (u ^. spdUserNachname)
      , "anrede"          .= (u ^. spdUserAnrede)
      , "mail"            .= (u ^. spdUserMail)
      , "funktion"        .= (u ^. spdUserFunktion)
      , "mitgliedsnummer" .= (u ^. spdUserMitgliedsnummer)
      , "telefon"         .= (u ^. spdUserTelefon)
      , "plz"             .= (u ^. spdUserPlz)
      , "stadt"           .= (u ^. spdUserStadt)
      , "strasse"         .= (u ^. spdUserStrasse)
      , "hausnummer"      .= (u ^. spdUserHausnummer)
      , "kandidaturen"    .= (u ^. spdUserKandidaturen)
      ]

instance Aeson.FromJSON MyFunktion where
  parseJSON = Aeson.withText "MyFunktion" $ pure . parseMyFunktion

parseMyFunktion :: ST -> MyFunktion
parseMyFunktion "Mitglied" = MyFunktionMitglied
parseMyFunktion s          = MyFunktionSonstige s

instance Aeson.ToJSON MyFunktion where
  toJSON = Aeson.String . fromMyFunktion

fromMyFunktion :: MyFunktion -> ST
fromMyFunktion MyFunktionMitglied     = "Mitglied"
fromMyFunktion (MyFunktionSonstige s) = s

instance Aeson.FromJSON MyKandidatur where
  parseJSON = Aeson.withObject "MyKandidatur" $ \ o ->
      MyKandidatur
        <$> o .: "id"
        <*> o .: "wahl"

instance Aeson.ToJSON MyKandidatur where
  toJSON k = Aeson.object
      [ "id"   .= (k ^. spdKandidaturId)
      , "wahl" .= (k ^. spdKandidaturWahl)
      ]

instance Aeson.FromJSON TelephoneNumber where
  parseJSON = (TelephoneNumber <$>) . Aeson.parseJSON

instance Aeson.ToJSON TelephoneNumber where
  toJSON = Aeson.toJSON . fromTelephoneNumber


-- * cassava

instance Cassava.FromRecord MyUser where
  parseRecord r
      | Vector.length r == 12 = MyUser
          <$> r .! 0
          <*> r .! 1
          <*> r .! 2
          <*> r .! 3
          <*> (r .! 4 >>= userEmailFromField)
          <*> r .! 5
          <*> r .! 6
          <*> r .! 7
          <*> r .! 8
          <*> r .! 9
          <*> r .! 10
          <*> r .! 11
          <*> pure []
      | otherwise = mzero

instance Cassava.ToRecord MyUser where
  toRecord (MyUser
             _id
             _vorname
             _nachname
             _anrede
             _mail
             _funktion
             _mitgliedsnummer
             _telefon
             _plz
             _stadt
             _strasse
             _hausnummer
             []) =
         Cassava.record
           [ Cassava.toField _id
           , Cassava.toField _vorname
           , Cassava.toField _nachname
           , Cassava.toField _anrede
           , userEmailToField _mail
           , Cassava.toField _funktion
           , Cassava.toField _mitgliedsnummer
           , Cassava.toField _telefon
           , Cassava.toField _plz
           , Cassava.toField _stadt
           , Cassava.toField _strasse
           , Cassava.toField _hausnummer
           ]
  toRecord u = error $ "instance ToRecord MyUser: kandidaturen list not empty: " ++ show u

instance Cassava.FromField MyFunktion where
  parseField = pure . parseMyFunktion . cs

instance Cassava.ToField MyFunktion where
  toField = cs . fromMyFunktion

instance Cassava.FromField TelephoneNumber where
  parseField = pure . TelephoneNumber . cs

instance Cassava.ToField TelephoneNumber where
  toField = cs . fromTelephoneNumber

-- | (to avoid orphan instances)
userEmailFromField :: LBS -> Cassava.Parser UserEmail
userEmailFromField = maybe mzero return . parseUserEmail . cs

-- | (to avoid orphan instances)
userEmailToField :: UserEmail -> SBS
userEmailToField = cs . fromUserEmail


-- * ftphs

fetchUserBase :: URI -> IO [MyUser]
fetchUserBase uri
    | uriScheme uri /= (Scheme "ftp") = throwIO $ BadURI "bad scheme" uri
    | uriQuery uri /= (Query [])      = throwIO $ BadURI "non-empty query" uri
    | isJust (uriFragment uri)        = throwIO $ BadURI "non-empty fragment" uri
    | otherwise = do
        auth :: Authority
            <- maybe (throwIO (BadURI "no authority" uri)) pure $ uriAuthority uri

        host :: String
            <- pure . cs . hostBS . authorityHost $ auth

        port :: PortNumber
            <- pure . maybe 21 (fromIntegral . portNumber) . authorityPort $ auth

        h <- FTP.connectFTP host port
          >>= \ (h, r) -> assertResult (`Connect` uri) r >> return h

        case authorityUserInfo auth of
            Nothing -> pure ()
            Just (UserInfo (cs -> name) (cs -> pass)) -> do
                FTP.login h name (Just pass) Nothing >>= assertResult (`Login` uri)

        -- FIXME: login errors are not caught here, but re-thrown.  something is still wrong with
        -- the exception types.  (is it the same with connect?)

        (raw :: [String], _)  -- FIXME: check result, throw Download.  write another helper function
                              -- to do this conveniently here.
            <- FTP.getlines h (cs $ uriPath uri)

        let parsed :: Either String [MyUser]
            parsed = Vector.toList <$> Cassava.decode Cassava.NoHeader (cs $ unlines raw)

        either (throwIO . (`Parse` uri)) return parsed


data Error =
    BadURI ST URI
  | Connect Int URI
  | Login Int URI
  | Download Int URI
  | Parse String URI
  -- | ...
  deriving (Eq, Show, Typeable)

instance Exception Error

assertResult :: (Int -> Error) -> FTP.FTPResult -> IO ()
assertResult e (code, _) = if code < 300 then return () else throwIO $ e code


-- | Call 'fetchUserBase' and update to database incrementally (remove all entries that have gone
-- from input, update changed entries, add new entries).  NOTE: MyUser.id and UserId are two
-- completely different animals.  New accounts can originate in either of the two sides, and then
-- need to be able to extend to the other side.
updateUserBase :: [MyUser] -> IO ()
updateUserBase = undefined

-- | Call 'updateUserBase', but log any exceptions (level 'CRITICAL').
updateUserBaseLogErrors :: [MyUser] -> IO ()
updateUserBaseLogErrors = undefined


-- * tests

-- /userinfos/{id}
test :: LBS
test = "{ \
       \ \"id\" : 4, \
       \ \"vorname\" : \"Max\", \
       \ \"nachname\" : \"Mustermann\", \
       \ \"anrede\" : \"Herr\", \
       \ \"mail\" : \"max.mustermann@irgendwo.de\", \
       \ \"funktion\" : \"Mitglied\", \
       \ \"mitgliedsnummer\" : \"1234567890\", \
       \ \"telefon\" : \"030-1234567\", \
       \ \"plz\" : \"12345\", \
       \ \"stadt\" : \"Musterhausen\", \
       \ \"strasse\" : \"Musterstrasse\", \
       \ \"hausnummer\" : \"1\", \
       \ \"kandidaturen\" : [ { \"id\" : 4, \"wahl\" : \"Musterwahl1\" }, { \"id\" : 5, \"wahl\" : \"Musterwahl2\" } ] }"


test0 :: Either String MyUser
test0 = Aeson.eitherDecode test

test1 :: Either String LBS
test1 = test0 >>= pure . Cassava.encode . (:[]) . (spdUserKandidaturen .~ [])

test2 :: IO ()
test2 = do
    x <- fetchUserBase ((\ (Right v) -> v) $ parseURI strictURIParserOptions someUri)
    print x




someUri :: SBS
someUri = "..."
