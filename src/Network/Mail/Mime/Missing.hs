{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS -fno-warn-orphans #-}

module Network.Mail.Mime.Missing
where

import Network.Mail.Mime

-- | See https://github.com/snoyberg/mime-mail/pull/38.  (Michael
-- Snoyman just merged this - it should be in the next release after
-- 0.4.7.)
deriving instance Eq Address
