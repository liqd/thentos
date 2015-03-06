{-# LANGUAGE OverloadedStrings                        #-}

module Thentos.Smtp
    ( sendUserConfirmationMail
    , sendUserExistsMail
) where

import Data.Monoid ((<>))
import Data.String.Conversions (cs, ST, LT)
import Network.Mail.Mime (Address(Address), renderSendMailCustom, simpleMail')
import System.Log (Priority(DEBUG))

import System.Log.Missing
import Thentos.Config (SmtpConfig(SmtpConfig))
import Thentos.Types

sendUserConfirmationMail :: SmtpConfig -> UserFormData -> LT -> IO ()
sendUserConfirmationMail smtpConfig user callbackUrl = do
    logger DEBUG $ "sending user-create-confirm mail: " ++ show user ++ " " ++ cs callbackUrl
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

sendMail :: SmtpConfig -> ST -> LT -> UserEmail -> IO ()
sendMail config subject message address = do
    renderSendMailCustom sendmailPath sendmailArgs mail
  where
    SmtpConfig sentFromAddress sendmailPath sendmailArgs = config
    mail = simpleMail' receiverAddress sentFromAddress subject message
    receiverAddress = Address Nothing (fromUserEmail $ address)
