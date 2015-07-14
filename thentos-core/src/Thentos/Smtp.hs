{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Thentos.Smtp
where

import Control.Applicative ((<$>))
import Control.Exception (try, IOException)
import Control.Monad (unless)
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

data SendmailError = SendmailError String

sendMail :: SmtpConfig -> Maybe UserName -> UserEmail -> ST -> ST -> IO (Either SendmailError ())
sendMail config mName address subject message = do
    logger DEBUG $ "sending email: " ++ ppShow (address, subject, message)
    renderedMail <- renderMail' mail
    r <- try $ sendmailCustomCaptureOutput sendmailPath sendmailArgs renderedMail
    case r of
        Right (out, err) -> do
            unless (SB.null out) .
                logger WARNING $ "sendmail produced output on std out: " ++ cs out
            unless (SB.null err) .
                logger WARNING $ "sendmail produced output on std err: " ++ cs err
            return $ Right ()
        Left (e :: IOException) ->
            return . Left . SendmailError $ "IO error running sendmail: " ++ show e
  where
    receiverAddress = Address (fromUserName <$> mName) (fromUserEmail $ address)
    sentFromAddress = buildEmailAddress config

    mail :: Mail
    mail = simpleMail' receiverAddress sentFromAddress subject (cs message)

    sendmailPath :: String   = cs  $  config >>. (Proxy :: Proxy '["sendmail_path"])
    sendmailArgs :: [String] = cs <$> config >>. (Proxy :: Proxy '["sendmail_args"])
