{-# LANGUAGE OverloadedStrings                        #-}

module Thentos.Smtp (sendUserConfirmationMail) where

import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Network.Mail.Mime (Address(Address), renderSendMailCustom, Mail(..), emptyMail, Part(..), Encoding(None))
import System.Log (Priority(DEBUG))

import qualified Data.ByteString.Lazy as L

import System.Log.Missing
import Thentos.Config
import Thentos.Types

sendUserConfirmationMail :: SmtpConfig -> UserFormData -> ByteString -> IO ()
sendUserConfirmationMail (SmtpConfig sentFromAddress sendmailPath sendmailArgs) user callbackUrl = do
    logger DEBUG $ "sending user-create-confirm mail: " ++ show user ++ " " ++ cs callbackUrl
    renderSendMailCustom sendmailPath sendmailArgs mail
  where
    mail = (emptyMail sentFromAddress)
            { mailTo = [receiverAddress]
            , mailHeaders = headers
            , mailParts = [body]
            }
    receiverAddress = Address Nothing (fromUserEmail $ udEmail user)
    message = "Please go to " <> L.fromStrict callbackUrl
    body = [Part { partType = "text"
                 , partFilename = Nothing
                 , partHeaders = []
                 , partContent = message
                 , partEncoding = None
                 }
           ]
    headers = [("Subject", "Thentos account creation confirmation")]
