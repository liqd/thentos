{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE InstanceSigs                #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE TypeOperators               #-}

module LIO.Missing
where

import LIO.Core (MonadLIO, liftLIO, taint, guardWrite)
import LIO.Error (AnyLabelError)
import LIO.Label (Label)
import LIO.DCLabel (DCLabel, (%%))

import qualified LIO.Exception as LE


tryTaint :: (MonadLIO l m, Label l) => l -> m r -> (AnyLabelError -> m r) -> m r
tryTaint label onSuccess onFailure = do
    result <- liftLIO $ LE.try (taint label)
    case result of
      Left e -> onFailure e
      Right () -> onSuccess

tryGuardWrite :: (MonadLIO l m, Label l) => l -> m r -> (AnyLabelError -> m r) -> m r
tryGuardWrite label onSuccess onFailure = do
    result <- liftLIO $ LE.try (guardWrite label)
    case result of
      Left e -> onFailure e
      Right () -> onSuccess

-- | Test whether guard-write against a given label violates current clearance.  In other words:
-- whether given label can flow to clearance.
guardWriteOk :: MonadLIO l m => l -> m Bool
guardWriteOk l = tryGuardWrite l (pure True) (\_ -> pure False)

dcBottom :: DCLabel
dcBottom = True %% False

dcTop :: DCLabel
dcTop = False %% True
