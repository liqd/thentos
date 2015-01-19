{-# LANGUAGE OverloadedStrings #-}

module Frontend.Pages (addUserPage, userForm, userAddedPage) where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html (Html)
import Text.Digestive.Blaze.Html5 (form, inputText, inputPassword, label, inputSubmit)
import Text.Digestive.Form (Form, check, text, (.:))
import Text.Digestive.View (View)

import Types

addUserPage :: View Html -> Html
addUserPage v = H.docTypeHtml $ do
    H.head $ do
        H.title "Create user"
    H.body $ do
        -- FIXME: how do we avoid having to duplicate the URL here?
        form v "create_user.html" $ do
            H.p $ do
                label "name" v "User name:"
                inputText "name" v
            H.p $ do
                label "password" v "Password: "
                inputPassword "password" v
            H.p $ do
                label "email" v "Email Address:"
                inputText "email" v
            inputSubmit "Create User"

userAddedPage :: Html
userAddedPage =
    H.docTypeHtml $ do
        H.head $
            H.title "Success!"
        H.body $ do
            H.h1 "Added a user!"

-- FIXME: move forms into separate module
userForm :: Monad m => Form Html m User
userForm = User
    <$> (Username <$> "name" .: check "name must not be empty" nonEmpty (text Nothing))
    <*> (UserPass <$> "password" .: check "password must not be empty" nonEmpty (text Nothing))
    <*> (UserEmail <$> "email" .: check "must be a valid email address" checkEmail (text Nothing))
    <*> pure []
    <*> pure []

  where
    nonEmpty :: Text -> Bool
    nonEmpty = not . T.null

    checkEmail :: Text -> Bool
    checkEmail = isJust . T.find (== '@')
