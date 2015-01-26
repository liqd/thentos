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

import Data.String.Conversions (ST)
import LIO.DCLabel (DCLabel, dcDefaultState, (%%))
import LIO.TCB (LIOState(LIOState))

import Types
import DB.Error
import DB.Core


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
mkAuth :: Maybe ST -> Maybe ST -> Maybe ST -> DB -> Either DbError Auth
mkAuth (Just user) Nothing        (Just password) db = Right allowEverything
mkAuth Nothing     (Just service) (Just password) db = Right allowEverything
mkAuth Nothing     Nothing        Nothing         db = Right allowEverything
mkAuth _           _              _               db = Left BadAuthenticationHeaders






allowNothing :: LIOState DCLabel
allowNothing = LIOState (False %% False) (True %% False)
  -- FIXME: is this correct?  what does it meaen?

allowEverything :: LIOState DCLabel
allowEverything = dcDefaultState
