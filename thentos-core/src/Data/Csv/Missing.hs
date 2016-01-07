{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Csv.Missing where

import Data.Csv
import Control.Monad (mzero)

instance ToField Bool where
    toField True = "1"
    toField False = "0"

instance FromField Bool where
    parseField "1" = pure True
    parseField "0" = pure False
    parseField _   = mzero
