module Util
    ( makeUserFromFormData
    , textToPassword
    , verifyPass
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Lens ((^.))
import Crypto.Scrypt (encryptPassIO', Pass(Pass), verifyPass')
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Types

makeUserFromFormData :: MonadIO m => UserFormData -> m User
makeUserFromFormData userData = do
    -- encryptPassIO' gets its entropy from /dev/urandom
    hashedPassword <- liftIO . encryptPassIO' . fromUserPass $ udPassword userData
    return $ User (udName userData)
                  (EncryptedPass hashedPassword)
                  (udEmail userData)
                  []
                  []

textToPassword :: Text -> UserPass
textToPassword = UserPass . Pass . encodeUtf8

verifyPass :: UserPass -> User -> Bool
verifyPass pass user = verifyPass' (fromUserPass pass)
                                   (fromEncryptedPass $ user ^. userPassword)
