{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Thentos.Smtp (sendMail, SendmailError(..), checkSendmail)
where

import Control.Exception (try, IOException, ErrorCall(..), throwIO)
import Control.Monad (unless, when)
import Data.Configifier ((>>.))
import Data.Maybe (fromJust, isJust)
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

sendMail :: SmtpConfig -> Maybe UserName -> UserEmail -> ST -> ST -> Maybe ST -> IO (Either SendmailError ())
sendMail config mName address subject message html = do
    logger DEBUG $ "sending email: " ++ ppShow (address, subject, message)
    when (isJust html) . logger WARNING $ "No support for the optional HTML part"
    renderedMail <- renderMail' mail
    r <- try $ sendmailCustomCaptureOutput sendmailPath sendmailArgs renderedMail
    case r of
        Right (out, err) -> do
            unless (SB.null out) .
                logger WARNING $ "sendmail produced output on stdout: " ++ cs out
            unless (SB.null err) .
                logger WARNING $ "sendmail produced output on stderr: " ++ cs err
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

-- | Run sendMail to check that we can send emails. Throw an error if sendmail
-- is not available or doesn't work.
checkSendmail :: SmtpConfig -> IO ()
checkSendmail cfg = do
    let address = fromJust $ parseUserEmail "user@example.com"
    result <- sendMail cfg Nothing address "Test Mail" "This is a test" Nothing
    case result of
        Left _ -> throwIO $ ErrorCall "sendmail seems to not work.\
                                        \ Maybe the sendmail path is misconfigured?"
        Right () -> return ()
