{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE DeriveDataTypeable                       #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TemplateHaskell                          #-}
{-# LANGUAGE TypeFamilies                             #-}
{-# LANGUAGE TypeOperators                            #-}
{-# LANGUAGE ViewPatterns                             #-}

{-# OPTIONS  #-}

module DB.Protect
  ( Auth
  , mkAuth
  , thentosLabeledPublic
  , thentosLabeledDenied
  , thentosPublic
  , thentosDenied
  , allowEverything
  , allowNothing
  ) where

import Control.Monad (void)
import Data.Acid (AcidState, UpdateEvent, QueryEvent, EventState, EventResult)
import Data.Acid.Advanced (update', query')
import Data.Functor.Infix ((<$>))
import Data.IORef (readIORef)
import Data.String.Conversions (ST)
import LIO (canFlowTo, lioClearance)
import LIO.DCLabel (DC, DCLabel, dcDefaultState, dcPublic, (%%))
import LIO.TCB (LIOState(LIOState), LIO(LIOTCB))

import Types
import DB.Error


-- | Result type of 'ThentosAuth'.  Contains authentication
-- information in the form required by @LIO@ in module "DB".
type Auth = LIOState DCLabel


-- | If password cannot be verified, or if only password or only
-- principal is provided, throw an error explaining the problem.  If
-- none are provided, set clearance level to 'allowNothing'.  If both
-- are provided, look up roles of principal, and set clearance level
-- to that of the principal aka agent and all its roles.
--
-- Note: Both 'Role's and 'Agent's can be used in authorization
-- policies.  ('User' can be used, but it must be wrapped into an
-- 'UserA'.)
mkAuth :: Maybe ST -> Maybe ST -> AcidState DB -> Auth
mkAuth _ _ _ = allowEverything
-- mkAuth (Just principal) (Just password) st = allowEverything


thentosLabeledPublic :: t -> ThentosLabeled t
thentosLabeledPublic = thentosLabeled dcPublic

thentosLabeledDenied :: t -> ThentosLabeled t
thentosLabeledDenied = error "thentosLabeledDenied: not implemented"

thentosPublic :: ThentosClearance
thentosPublic = ThentosClearance $ ThentosLabel dcPublic

thentosDenied :: ThentosLabel
thentosDenied = error "thentosLabeledDenied: not implemented"

allowNothing :: LIOState DCLabel
allowNothing = LIOState (False %% False) (True %% False)
  -- FIXME: is this correct?  what does it meaen?

allowEverything :: LIOState DCLabel
allowEverything = dcDefaultState
