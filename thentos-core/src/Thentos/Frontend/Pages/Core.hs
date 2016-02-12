{-# LANGUAGE OverloadedStrings #-}
module Thentos.Frontend.Pages.Core where

import Text.Blaze.Html (Html, (!))
import Text.Digestive.Blaze.Html5 (form, childErrorList)
import Text.Digestive.Form (Form, check, validate)
import Text.Digestive.Types (Result(Success, Error))
import Text.Digestive.View (View, absoluteRef)

import qualified Data.Text as ST
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Thentos.Frontend.Types
import Thentos.Prelude
import Thentos.Types

-- * base layout

-- | Call 'basePagelet'' without optional headings.
basePagelet :: FrontendSessionData -> ST -> Html -> Html
basePagelet fsd title = basePagelet' fsd title Nothing

-- | Create an html document with default headings from title,
-- optional headings, and body.
basePagelet' :: FrontendSessionData -> ST -> Maybe Html -> Html -> Html
basePagelet' fsd title mHeadings body = H.docTypeHtml $ do
    H.head $ do
        H.title $ H.text title
        H.link ! A.rel "stylesheet" ! A.href "/screen.css"
        sequence_ mHeadings
    H.body $ do
        H.div . H.ul $ for_ (fsd ^. fsdMessages) (H.li . H.string . show)
        H.h1 $ H.text title
        body


-- * forms

-- | Protect a form from CSRF attacks by including a secret token as a hidden field.
csrfProofForm :: FrontendSessionData -> View Html -> ST -> Html -> Html
csrfProofForm fsd v action f = do
    childErrorList "" v
    form v action $ f <> csrfField
  where
    csrfField
      | Just csrfToken <- fsd ^. fsdCsrfToken =
          let name = H.toValue (absoluteRef "_csrf" v) in
          H.input   ! A.type_ "hidden"
                    ! A.id    name
                    ! A.name  name
                    ! A.value (H.toValue (fromCsrfToken csrfToken))
      | otherwise =
          mempty


-- * error / status reports to the user

errorPage :: String -> Html
errorPage = basePagelet fsd "Error" . errorHtml
  where
    fsd = emptyFrontendSessionData

errorPagelet :: u -> rs -> String -> Html
errorPagelet _ _ = errorHtml

errorHtml :: String -> Html
errorHtml = H.string . ("*** error: " ++) . show

permissionDeniedPage :: Html
permissionDeniedPage = basePagelet' fsd "Permission Denied" Nothing
    (H.a ! A.href "/dashboard" $ "Back to dashboard")
  where
    fsd = emptyFrontendSessionData

notFoundPage :: Html
notFoundPage = basePagelet fsd "Not Found" $ H.p "The requested page does not exist."
  where
    fsd = emptyFrontendSessionData

confirmationMailSentPage :: FrontendSessionData -> ST -> ST -> ST -> Html
confirmationMailSentPage fsd title msg1 msg2 =
    basePagelet fsd title $ confirmationMailSentBody msg1 msg2

confirmationMailSentSnippet :: ST -> ST -> u -> rs -> Html
confirmationMailSentSnippet msg1 msg2 _ _ = confirmationMailSentBody msg1 msg2

confirmationMailSentBody :: ST -> ST -> Html
confirmationMailSentBody msg1 msg2 = H.p . H.text . ST.unlines $
    msg1 :
    "Please check your email (don't forget the spam folder)" :
    "and complete " <> msg2 <> " by following the link we sent you." :
    []


-- * form field validation

validateNonEmpty :: (Monoid v, IsString v, Monad m) => v -> Form v m ST -> Form v m ST
validateNonEmpty fieldName = check (fieldName <> " must not be empty") (not . ST.null)

validateEmail :: (Monoid v, IsString v, Monad m) => Form v m ST -> Form v m UserEmail
validateEmail = validate $ maybe (Error "email address invalid") Success . parseUserEmail

validatePass :: (UserPass, UserPass) -> Result Html UserPass
validatePass (p1, p2)
    | p1 == p2  = Success p1
    | otherwise = Error "passwords don't match"

validatePassChange :: (UserPass, UserPass, UserPass) -> Result Html (UserPass, UserPass)
validatePassChange (old, new1, new2)
    | new1 == new2 = Success (old, new1)
    | otherwise    = Error "passwords don't match"
