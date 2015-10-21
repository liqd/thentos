module Thentos.Frontend.Errors where

import Thentos.Types
import Text.Blaze

-- Use other monad instead of action in handlers? ExceptT ServantErr IO
data FrontendError = FrontendError

generic500 :: Markup
generic500 = html $ do
    head $ do
        title "Something went wrong!"
    body $ do
        div $ p "Something went wrong!"

renderErrs :: Action FrontendError :~> ExceptT ServantErr IO
renderErrs = Nat go
  where go


instance ToMarkup FrontendError where
    {- toMarkup -}

instance ToMarkup (ThentosError FrontendError) where
    toMarkup UserEmailAlreadyExists =
    toMarkup (OtherError e) = toMarkup e
    toMarkup e = generic500
