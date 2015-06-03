{-# LANGUAGE OverloadedStrings #-}

-- | The contents of this module are copied from the snap-extras package to
-- avoid several large dependencies of snap-extras that we're not using at all.
module Snap.Inline (blanketCSRF, handleCSRF) where

import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding    as T
import           Snap
import           Snap.Snaplet.Session

------------------------------------------------------------------------------
-- | Use this function to wrap your whole site with CSRF protection.  Due to
-- security considerations, the way Snap parses file uploads
-- means that the CSRF token cannot be checked before the file uploads have
-- been handled.  This function protects your whole site except for handlers
-- of multipart/form-data forms (forms with file uploads).  To protect those
-- handlers, you have to call handleCSRF explicitly after the file has been
-- processed.
blanketCSRF :: SnapletLens v SessionManager
            -- ^ Lens to the session snaplet
            -> Handler b v ()
            -- ^ Handler to run if the CSRF check fails
            -> Handler b v ()
            -- ^ Handler to let through when successful.
            -> Handler b v ()
blanketCSRF session onFailure onSucc = do
    h <- getHeader "content-type" `fmap` getRequest
    case maybe False (B.isInfixOf "multipart/form-data") h of
      True -> onSucc
      False -> handleCSRF session onFailure onSucc


------------------------------------------------------------------------------
-- | If a request is a POST, check the CSRF token and fail with the specified
-- handler if the check fails.  If if the token is correct or if it's not a
-- POST request, then control passes through as a no-op.
handleCSRF :: SnapletLens v SessionManager
           -- ^ Lens to the session snaplet
           -> Handler b v ()
           -- ^ Handler to run on failure
           -> Handler b v ()
           -- ^ Handler to let through when successful.
           -> Handler b v ()
handleCSRF session onFailure onSucc = do
    m <- getsRequest rqMethod
    case m /= POST of
      True ->  onSucc
      False -> do
        tok <- getParam "_csrf"
        realTok <- with session csrfToken
        if tok == Just (T.encodeUtf8 realTok)
          then onSucc
          else onFailure >> getResponse >>= finishWith


