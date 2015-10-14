{-# LANGUAGE Unsafe                      #-}

{-# LANGUAGE ConstraintKinds             #-}
{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveFunctor               #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE InstanceSigs                #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE PackageImports              #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE UndecidableInstances        #-}

{-| Simplified access to 'Action' with guarded exits.

-}
module Thentos.Action.SimpleCheckClearance
  ( UnsafeAction(..)
  , makeActionSafe
  , withUserIsLoggedIn
  , withUserHasId
  , withUserHasRole
  ) where

import Control.Lens ((^.))
import Control.Monad.Except (MonadError, throwError, catchError)
import Control.Monad.Except (throwError, catchError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT(ReaderT), MonadReader, runReaderT, ask)
import Control.Monad.Trans.Either (EitherT(EitherT), eitherT)
import Control.Monad (unless, void)
import Data.Configifier ((>>.), Tagged(Tagged))
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (ST, cs)
import Data.Typeable (Typeable)
import GHC.Exception (Exception)
import GHC.Generics
import LIO.Core
import LIO.DCLabel ((%%), (\/), (/\))
import LIO.Error (AnyLabelError)
import LIO.TCB
import System.Log.Logger (Priority(DEBUG))

import qualified Codec.Binary.Base64 as Base64
import qualified Data.Text as ST

import LIO.Missing
import Thentos.Action.Core
import Thentos.Types
import Thentos.Util

import qualified Thentos.Transaction as T
import Thentos.Transaction.Core (ThentosQuery)


-- | Like 'Action', but with 'IO' at the base.
newtype UnsafeAction e a =
    UnsafeAction
      { fromUnsafeAction :: ReaderT ActionState (EitherT (ThentosError e) IO) a
      }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader ActionState
           , MonadError (ThentosError e)
           , MonadIO
           , Typeable
           , Generic
           )

-- | Run an 'UnsafeAction' in a safe 'Action' without extra authorization checks.
makeActionSafe :: UnsafeAction e a -> Action e a
makeActionSafe = undefined

-- | Check that this request comes from a logged-in user.
withUserIsLoggedIn :: UnsafeAction e a -> Action e a
withUserIsLoggedIn = withUserHasRole RoleUser

-- | Check that the logged-in user has the expected 'UserID'.
withUserHasId :: UserId -> UnsafeAction e a -> Action e a
withUserHasId _ _ = error "withUserHasId"

-- | Check that the logged-in user belongs to a given 'Role'.
withUserHasRole :: Role -> UnsafeAction e a -> Action e a
withUserHasRole _ _ = error "withUserHasRole"
