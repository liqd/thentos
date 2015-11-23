module Error where

foreign import throwJS :: forall e a. e -> a
foreign import warnJS :: forall e a. e -> a -> a
