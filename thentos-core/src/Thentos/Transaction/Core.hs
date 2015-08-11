module Thentos.Transaction.Core
    ( ThentosUpdate(..)
    , ThentosQuery(..)
    )
where

newtype ThentosUpdate a = ThentosUpdate {fromThentosUpdate :: IO a}
newtype ThentosQuery a = ThentosQuery {fromThentosQuery :: IO a}
