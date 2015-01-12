{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE DeriveDataTypeable                       #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TemplateHaskell                          #-}
{-# LANGUAGE TypeFamilies                             #-}
{-# LANGUAGE TypeOperators                            #-}
{-# LANGUAGE ViewPatterns                             #-}

{-# OPTIONS  #-}

module DB.Protected
  ( queryLIO
  , updateLIO
  , updateLIO_
  ) where

import Control.Monad (void)
import Data.Acid (AcidState, UpdateEvent, QueryEvent, EventState, EventResult)
import Data.Acid.Advanced (update', query')
import Data.Functor.Infix ((<$>))
import Data.IORef (readIORef)
import LIO (canFlowTo, lioClearance)
import LIO.DCLabel (DCLabel)
import LIO.TCB (LIO(LIOTCB))

import Types


-- | Run a query action that returns a labeled result.  Check that the
-- clearance is sufficient for the label, and return the unlabelled
-- result.
queryLIO :: (QueryEvent event, EventResult event ~ Labeled a) => AcidState (EventState event) -> event -> LIO DCLabel a
queryLIO st ev = LIOTCB $ \ stateRef -> do
  clearance :: DCLabel <- lioClearance <$> readIORef stateRef
  LabeledTCB (context :: DCLabel) result <- query' st ev
  -- FIXME: put in @taint clearance@ here, or something.
  if context `canFlowTo` clearance  -- FIXME: or is it supposed flow the other way?
    then return result
    else fail "authorization denied"  -- FIXME: throw error type here.

    -- NOTE: auth errors must always be caught first, before any other
    -- errors, or else non-authorization errors may leak information
    -- to unauthorized parties.

-- | Like 'queryLIO'.  If an action is not authorized, it will be
-- undone after the fact.  Not ideal, but lazyness may save us here.
-- An alternative implementation would create a type class, make all
-- 'UpdateEvent's instances, and ask the instances for the label.
-- That could be done before the operation, but then we would have no
-- dynamic context to compute the label with.
updateLIO :: (UpdateEvent event, EventResult event ~ Labeled a) => AcidState (EventState event) -> event -> LIO DCLabel a
updateLIO st ev = LIOTCB $ \ stateRef -> do
  clearance :: DCLabel <- lioClearance <$> readIORef stateRef
  LabeledTCB (context :: DCLabel) result <- update' st ev
  -- FIXME: put in @taint clearance@ here, or something.
  if context `canFlowTo` clearance
    then return result
    else fail "authorization denied"

-- | Call 'updateLIO' and discard the result.
updateLIO_ :: (UpdateEvent event, EventResult event ~ Labeled a) => AcidState (EventState event) -> event -> LIO DCLabel ()
updateLIO_ st = void . updateLIO st
