{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Thentos.Smtp
where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.Configifier ((>>.))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (ST, cs)
import Network.Mail.Mime (Mail, Address(Address), sendmailCustomCaptureOutput,
    simpleMail', renderMail')
import System.Log (Priority(DEBUG, WARNING))
import Text.Show.Pretty (ppShow)

import qualified Data.ByteString as SB

import System.Log.Missing
import Thentos.Config
import Thentos.Types

sendMail :: SmtpConfig -> Maybe UserName -> UserEmail -> ST -> ST -> IO ()
sendMail config mName address subject message = do
    logger DEBUG $ "sending email: " ++ ppShow (address, subject, message)
    renderedMail <- renderMail' mail
    (out, err) <- sendmailCustomCaptureOutput sendmailPath sendmailArgs renderedMail
    when (not $ SB.null out) $
        logger WARNING $ "sendmail produced output on std out: " ++ cs out
    when (not $ SB.null err) $
        logger WARNING $ "sendmail produced output on std err: " ++ cs err
  where
    receiverAddress = Address (fromUserName <$> mName) (fromUserEmail $ address)
    sentFromAddress = buildEmailAddress config

    mail :: Mail
    mail = simpleMail' receiverAddress sentFromAddress subject (cs message)

    sendmailPath :: String   = cs  $  config >>. (Proxy :: Proxy '["sendmail_path"])
    sendmailArgs :: [String] = cs <$> config >>. (Proxy :: Proxy '["sendmail_args"])
