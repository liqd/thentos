module Control.Monad.Except.Missing where

import Control.Monad.Except (MonadError(catchError, throwError))
import Data.Functor (($>))

finally :: MonadError e m => m a -> m b -> m a
finally action finalizer = do
    a <- action `catchError` \e -> finalizer >> throwError e
    finalizer $> a
