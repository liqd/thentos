module Error where

import Prelude

foreign import throwJS :: forall e a. e -> a
foreign import warnJS :: forall e a. e -> a -> a
