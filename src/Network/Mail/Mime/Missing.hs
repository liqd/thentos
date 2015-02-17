{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS -fno-warn-orphans #-}

module Network.Mail.Mime.Missing
where

import Network.Mail.Mime

-- | See https://github.com/snoyberg/mime-mail/pull/38.
deriving instance Eq Address
