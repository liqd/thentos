{-# LANGUAGE OverloadedStrings                        #-}

module Frontend.Mail (sendUserConfirmationMail) where

import Control.Lens ((^.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Monoid ((<>))
import Network.Mail.Mime (Address(Address), renderSendMail, Mail(..), emptyMail,
    Part(..), Encoding(None))

import Types

sentFromAddress :: Address
sentFromAddress = Address (Just "Thentos") "thentos@thentos.org"

sendUserConfirmationMail :: User -> ByteString -> IO ()
sendUserConfirmationMail user callbackUrl = do
    renderSendMail mail
  where
    mail = (emptyMail sentFromAddress)
            { mailTo = [receiverAddress]
            , mailHeaders = headers
            , mailParts = [body]
            }
    receiverAddress = Address Nothing (fromUserEmail $ user ^. userEmail)
    message = "Please go to " <> L.fromStrict callbackUrl
    body = [Part { partType = "text"
                 , partFilename = Nothing
                 , partHeaders = []
                 , partContent = message
                 , partEncoding = None
                 }
           ]
    headers = [("Subject", "Thentos account creation confirmation")]
