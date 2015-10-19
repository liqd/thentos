{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Thentos.Frontend.HandlersS (registerServer, RegisterAPI) where

import Control.Lens ((^.), (.~), (%~))
import           Data.Monoid                 ((<>))
import           Data.String.Conversions     (ST, cs)
import           Data.Void                   (Void)
import           Servant
import           Servant.HTML.Blaze          (HTML)
import           Servant.Missing             (FormGet, FormPost, HasForm (..))
import           System.Log                  (Priority (DEBUG, INFO, WARNING, CRITICAL))
import System.Log.Missing (logger)
import           Text.Blaze.Html             (Html, ToValue (toValue), (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Digestive.Blaze.Html5  (inputPassword, inputSubmit,
                                              inputText, label)
import           Text.Digestive.View         (View)
import           Thentos.Config
import           Thentos.Frontend.Handlers.Combinators (urlConfirm)

import           Thentos.Action              as A
import           Thentos.Action.Core
import           Thentos.Frontend.Pages
import           Thentos.Frontend.Types
import           Thentos.Types

csrf = undefined
formAction = undefined
backend = undefined
smtpConfig = undefined
feConfig = undefined
sendFrontendMsg = undefined
modifySessionData' = undefined

type RegisterAPI = FormGet "Register" H.Html UserFormData
              :<|> FormPost "Register" H.Html UserFormData :> Post '[HTML] H.Html

registerServer :: ServerT RegisterAPI (Action Void)
registerServer = registerUserGet :<|> registerUserPost


-- * Handlers

registerUserGet :: Monad m => m ()
registerUserGet = return ()

registerUserPost :: UserFormData -> Action Void Html
registerUserPost ufd = do
    (_, tok) <- addUnconfirmedUser ufd
    let url = urlConfirm feConfig "/user/register_confirm" $ fromConfirmationToken tok
    sendUserConfirmationMail smtpConfig ufd url
    return userRegisterRequestedPage

registerUserConfirm :: Action Void Html
registerUserConfirm = do
    (uid, sessTok) <- confirmNewUser token
    logger DEBUG $ "registered new user: " ++ show uid
    mapM_ (assignRole (UserA uid)) defaultUserRoles
    sendFrontendMsg $ FrontendMsgSuccess "Registration complete.  Welcome to Thentos!"
    modifySessionData' $ fsdLogin .~ Just (FrontendSessionLoginData sessionToken uid)
    redirectToDashboardOrService



-- * HasForm

instance HasForm "Register" H.Html UserFormData where
    isForm _   = userRegisterForm
    formView _ = registerFormView

registerFormView :: View H.Html -> H.Html
registerFormView v = do
    H.p $ do
        label "name" v "User name:"
        inputText "name" v
    H.p $ do
        label "password1" v "Password:"
        inputPassword "password1" v
    H.p $ do
        label "password2" v "Repeat Password:"
        inputPassword "password2" v
    H.p $ do
        label "email" v "Email Address:"
        inputText "email" v
    inputSubmit "Create User" ! A.id "create_user_submit"

-- * Utils

sendUserConfirmationMail :: SmtpConfig -> UserFormData -> ST -> Action Void ()
sendUserConfirmationMail smtpConfig user callbackUrl = do
    sendMail'P smtpConfig Nothing (udEmail user) subject message
  where
    message = "Please go to " <> callbackUrl <> " to confirm your account."
    subject = "Thentos account creation confirmation"

sendUserExistsMail :: SmtpConfig -> UserEmail -> Action Void ()
sendUserExistsMail smtpConfig address = do
    sendMail'P smtpConfig Nothing address subject message
  where
    message = "Someone tried to sign up to Thentos with your email address"
                <> "\nThis is a reminder that you already have a Thentos"
                <> " account. If you haven't tried to sign up to Thentos, you"
                <> " can just ignore this email. If you have, you are hereby"
                <> " reminded that you already have an account."
    subject = "Attempted Thentos Signup"
