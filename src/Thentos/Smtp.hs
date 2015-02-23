{-# LANGUAGE OverloadedStrings                        #-}

module Thentos.Smtp (sendUserConfirmationMail) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Monoid ((<>))
import Network.Mail.Mime (Address(Address), renderSendMail, Mail(..), emptyMail,
    Part(..), Encoding(None))

import Thentos.Types

sendUserConfirmationMail :: Address -> UserFormData -> ByteString -> IO ()
sendUserConfirmationMail sentFromAddress user callbackUrl = do
    renderSendMail mail
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
