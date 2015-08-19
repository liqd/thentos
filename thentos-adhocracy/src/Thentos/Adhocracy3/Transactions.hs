{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS -fno-warn-orphans #-}

module Thentos.Adhocracy3.Transactions where

import Control.Applicative ((<$>))
import Control.Lens ((%~), (^.))
import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.State (get, modify)

import Thentos.Adhocracy3.Types
import Thentos.Transaction.Core
import Thentos.Transaction.TH

import qualified Data.Set as Set

import qualified Thentos.Transaction as CoreTransaction
import qualified Thentos.Types as CoreTypes


-- * SSO

-- | Add an SSO token to the database
trans_addSsoToken :: (db `Extends` DB) => SsoToken -> ThentosUpdate db ()
trans_addSsoToken tok = polyUpdate . modify $ dbSsoTokens %~ Set.insert tok

-- | Remove an SSO token from the database. Throw NoSuchToken if the token doesn't exist.
trans_lookupAndRemoveSsoToken :: (db `Extends` DB) => SsoToken -> ThentosUpdate db ()
trans_lookupAndRemoveSsoToken tok = polyUpdate $ do
    exists <- Set.member tok . (^. dbSsoTokens) <$> get
    unless exists $ throwError SsoErrorUnknownCsrfToken
    modify $ dbSsoTokens %~ Set.delete tok

$(makeThentosAcidicPhase1 ''DB ['trans_addSsoToken, 'trans_lookupAndRemoveSsoToken])
$(makeThentosAcidicPhase2 ''DB
                          ['trans_addSsoToken, 'trans_lookupAndRemoveSsoToken]
                          [''CoreTypes.DB]
                          ['CoreTransaction.dbEvents])
