{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Thentos.Smtp
where

import Control.Applicative ((<$>))
import Data.Configifier ((>>.))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (ST, cs)
import Network.Mail.Mime (Mail, Address(Address), renderSendMailCustom, simpleMail')
import System.Log (Priority(DEBUG))
import Text.Show.Pretty (ppShow)

import System.Log.Missing
import Thentos.Config
import Thentos.Types

sendMail :: SmtpConfig -> Maybe UserName -> UserEmail -> ST -> ST -> IO ()
sendMail config mName address subject message = do
    logger DEBUG $ "sending email: " ++ ppShow (address, subject, message)
    renderSendMailCustom sendmailPath sendmailArgs mail
  where
    receiverAddress = Address (fromUserName <$> mName) (fromUserEmail $ address)
    sentFromAddress = buildEmailAddress config

    mail :: Mail
    mail = simpleMail' receiverAddress sentFromAddress subject (cs message)

    sendmailPath :: String   = cs  $  config >>. (Proxy :: Proxy '["sendmail_path"])
    sendmailArgs :: [String] = cs <$> config >>. (Proxy :: Proxy '["sendmail_args"])
