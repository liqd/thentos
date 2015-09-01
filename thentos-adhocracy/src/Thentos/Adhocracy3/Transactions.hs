module Thentos.Adhocracy3.Transactions where

import Thentos.Adhocracy3.Types
import Thentos.Transaction.Core

-- * SSO

-- | Add an SSO token to the database
addSsoToken :: SsoToken -> ThentosQuery ThentosA3Error ()
addSsoToken = error "addSsoToken not implemented"

-- | Remove an SSO token from the database. Throw NoSuchToken if the token doesn't exist.
lookupAndRemoveSsoToken :: SsoToken -> ThentosQuery ThentosA3Error ()
lookupAndRemoveSsoToken = error "lookupAndRemoveSsoToken not implemented"
