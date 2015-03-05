{-# LANGUAGE OverloadedStrings                        #-}

module Thentos.Smtp
    ( sendUserConfirmationMail
    , sendUserExistsMail
) where

import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Network.Mail.Mime (Address(Address), renderSendMailCustom, Mail(..), emptyMail, Part(..), Encoding(None))
import System.Log (Priority(DEBUG))

import qualified Data.ByteString.Lazy as L

import System.Log.Missing
import Thentos.Config (SmtpConfig(SmtpConfig))
import Thentos.Types

sendUserConfirmationMail :: SmtpConfig -> UserFormData -> ByteString -> IO ()
sendUserConfirmationMail smtpConfig user callbackUrl = do
    logger DEBUG $ "sending user-create-confirm mail: " ++ show user ++ " " ++ cs callbackUrl
    sendMail smtpConfig subject message (udEmail user)
  where
    message = "Please go to " <> L.fromStrict callbackUrl
    subject = "Thentos account creation confirmation"

sendUserExistsMail :: SmtpConfig -> UserEmail -> IO ()
sendUserExistsMail smtpConfig address = do
    logger DEBUG $ "sending user-already-exists mail: " ++ show address
    sendMail smtpConfig subject message address
  where
    message = "Someone tried to sign up to Thentos with your email address"
                <> "\nThis is a reminder that you already have a Thentos"
                <> " account. If you haven't tried to sign up to Thentos, you"
                <> " can just ignore this email."
    subject = "Attempted Thentos Signup"

-- FIXME: when we have more interesting emails, the message body should be Text
sendMail :: SmtpConfig -> Text -> L.ByteString -> UserEmail -> IO ()
sendMail config subject message address = do
    renderSendMailCustom sendmailPath sendmailArgs mail
  where
    SmtpConfig sentFromAddress sendmailPath sendmailArgs = config
    mail = (emptyMail sentFromAddress)
            { mailTo = [receiverAddress]
            , mailHeaders = headers
            , mailParts = [body]
            }
    receiverAddress = Address Nothing (fromUserEmail $ address)
    body = [Part { partType = "text"
                 , partFilename = Nothing
                 , partHeaders = []
                 , partContent = message
                 , partEncoding = None
                 }
           ]
    headers = [("Subject", subject)]
