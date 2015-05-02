{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Thentos.Smtp
    ( sendUserConfirmationMail
    , sendUserExistsMail
    , sendPasswordResetMail
    , sendEmailChangeConfirmationMail
) where

import Control.Applicative ((<$>))
import Control.Lens ((^.))
import Data.Configifier ((>>.))
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (ST, cs)
import Network.Mail.Mime (Address(Address), renderSendMailCustom, simpleMail')
import System.Log (Priority(DEBUG))

import System.Log.Missing
import Thentos.Config
import Thentos.Types

sendUserConfirmationMail :: SmtpConfig -> UserFormData -> ST -> IO ()
sendUserConfirmationMail smtpConfig user callbackUrl = do
    logger DEBUG $ "sending user-create-confirm mail: " ++ show (udEmail user)
    sendMail smtpConfig subject message (udEmail user)
  where
    message = "Please go to " <> callbackUrl <> " to confirm your account."
    subject = "Thentos account creation confirmation"


sendUserExistsMail :: SmtpConfig -> UserEmail -> IO ()
sendUserExistsMail smtpConfig address = do
    logger DEBUG $ "sending user-already-exists mail: " ++ show address
    sendMail smtpConfig subject message address
  where
    message = "Someone tried to sign up to Thentos with your email address"
                <> "\nThis is a reminder that you already have a Thentos"
                <> " account. If you haven't tried to sign up to Thentos, you"
                <> " can just ignore this email. If you have, you are hereby"
                <> " reminded that you already have an account."
    subject = "Attempted Thentos Signup"


sendPasswordResetMail :: SmtpConfig -> User -> ST -> IO ()
sendPasswordResetMail smtpConfig user callbackUrl = do
    logger DEBUG $ "sending password-reset email: " ++ show (user ^. userEmail)
    sendMail smtpConfig subject message (user ^. userEmail)
  where
    message = "To set a new password, go to " <> callbackUrl
    subject = "Thentos Password Reset"


sendEmailChangeConfirmationMail :: SmtpConfig -> UserEmail -> ST -> IO ()
sendEmailChangeConfirmationMail smtpConfig address callbackUrl = do
    logger DEBUG $ "sending email change confirmation email: " ++ show address
    sendMail smtpConfig subject message address
  where
    message = "Please go to " <> callbackUrl <>
                " to confirm your change of email address."
    subject = "Thentos email address change"


sendMail :: SmtpConfig -> ST -> ST -> UserEmail -> IO ()
sendMail config subject message address = do
    renderSendMailCustom sendmailPath sendmailArgs mail
  where
    sentFromAddress = buildEmailAddress config
    sendmailPath :: String = cs $ config >>. (Proxy :: Proxy '["sendmail_path"])
    sendmailArgs :: [String] = cs <$> config >>. (Proxy :: Proxy '["sendmail_args"])
    mail = simpleMail' receiverAddress sentFromAddress subject (cs message)
    receiverAddress = Address Nothing (fromUserEmail $ address)
