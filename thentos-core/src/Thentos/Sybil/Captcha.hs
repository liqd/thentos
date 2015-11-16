{-# LANGUAGE OverloadedStrings    #-}

module Thentos.Sybil.Captcha where

import Data.String.Conversions (ST)

import Thentos.Types


-- | Generate a captcha. Returns a pair of the binary image data in PNG format and the correct
-- solution to the captcha.
-- FIXME Implement by delegating to hs-captcha or whatever captcha library we'll end up using
generateCaptcha :: Random20 -> (ImageData, ST)
generateCaptcha _random = ("", "")
