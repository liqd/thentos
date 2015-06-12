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

import Control.Exception (Exception)
import LIO.Core (MonadLIO, liftLIO, taint)
import LIO.Label (Label)
import LIO.DCLabel (DCLabel, (%%))

import qualified LIO.Exception as LE


tryTaint :: (MonadLIO l m, Label l, Exception e) => l -> m r -> (e -> m r) -> m r
tryTaint label onSuccess onFailure = do
    result <- liftLIO $ LE.try (taint label)
    case result of
      Left e -> onFailure e
      Right () -> onSuccess

dcBottom :: DCLabel
dcBottom = True %% False

dcTop :: DCLabel
dcTop = False %% True
