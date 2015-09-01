module Thentos.Adhocracy3.Transactions where

import Control.Applicative ((<$>))
import Control.Lens ((%~), (^.))
import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.State (get, modify)

import Thentos.Adhocracy3.Types
import Thentos.Transaction.Core

import qualified Data.Set as Set

import qualified Thentos.Transaction as CoreTransaction
import qualified Thentos.Types as CoreTypes


-- * SSO

-- | Add an SSO token to the database
addSsoToken :: SsoToken -> ThentosQuery ()
addSsoToken = error "addSsoToken not implemented"

-- | Remove an SSO token from the database. Throw NoSuchToken if the token doesn't exist.
lookupAndRemoveSsoToken :: SsoToken -> ThentosQuery ()
lookupAndRemoveSsoToken = error "lookupAndRemoveSsoToken not implemented"
