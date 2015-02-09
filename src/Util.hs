module Util (makeUserFromFormData) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Scrypt (encryptPassIO')
import Types

makeUserFromFormData :: MonadIO m => UserFormData -> m User
makeUserFromFormData userData = do
    -- encryptPassIO' gets its entropy from /dev/urandom
    hashedPassword <- liftIO . encryptPassIO' $ udPassword userData
    return $ User (udName userData)
                  (EncryptedPass hashedPassword)
                  (udEmail userData)
                  []
                  []
