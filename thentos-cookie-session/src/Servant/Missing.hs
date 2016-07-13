-- FIXME: create a package servant-digestive-functors and
--        choose a different module name.
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.Missing
  (ThrowServantErr(..)
  ,MonadServantErr
  ,ThrowError500(..)
  ,MonadError500
  ,FormH
  ,FormReqBody
  ,FormData, getFormDataEnv, releaseFormTempFiles
  ,formH
  ,formRedirectH
  ,fromEnvIdentity
  ,redirect) where

import Control.Lens (prism, Prism', (#))
import Control.Monad ((>=>))
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Except.Missing (finally)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (InternalState, createInternalState, closeInternalState)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (ST, SBS, ConvertibleStrings, cs)
import Network.Wai.Parse (fileContent, parseRequestBody, tempFileBackEnd)
import Servant ((:<|>)((:<|>)), (:>), ServerT, (:~>)(Nat), unNat)
import Servant.Server (ServantErr(..), err500)
import Servant.Server.Internal (HasServer, route, addBodyCheck)
import Servant.Server.Internal.RoutingApplication (withRequest)
import Text.Digestive (Env, Form, FormInput(TextInput, FileInput), View, fromPath, getForm, postForm)

import qualified Servant
import qualified Data.Text.Encoding as STE

class ThrowServantErr err where
    _ServantErr :: Prism' err ServantErr
    throwServantErr :: MonadError err m => ServantErr -> m any
    throwServantErr err = throwError $ _ServantErr # err

type MonadServantErr err m = (MonadError err m, ThrowServantErr err)

instance ThrowServantErr ServantErr where
    _ServantErr = id

class ThrowError500 err where
    error500 :: Prism' err String

    throwError500 :: MonadError err m => String -> m b
    throwError500 err = throwError $ error500 # err

type MonadError500 err m = (MonadError err m, ThrowError500 err)

-- FIXME: ORPHAN move
instance ThrowError500 ServantErr where
    error500 = prism (\msg -> err500 { errBody = cs msg })
                     (\err -> if errHTTPCode err == 500 then Right (cs (errBody err)) else Left err)


type FormH (htm :: [*]) html payload =
         Servant.Get htm html
    :<|> FormReqBody :> Servant.Post htm html

data FormReqBody

fromEnvIdentity :: Applicative m => Env Identity -> Env m
fromEnvIdentity env = pure . runIdentity . env

data FormData = FormData
  { _formEnv :: Env Identity
  , _formTmpFilesState :: InternalState
  }

getFormDataEnv :: FormData -> Env Identity
getFormDataEnv (FormData env _) = env

releaseFormTempFiles :: FormData -> IO ()
releaseFormTempFiles (FormData _ tmpFilesState) = closeInternalState tmpFilesState

instance HasServer sublayout context => HasServer (FormReqBody :> sublayout) context where
  type ServerT (FormReqBody :> sublayout) m = FormData -> ServerT sublayout m

  route Proxy context subserver =
      route (Proxy :: Proxy sublayout) context (addBodyCheck subserver bodyCheck)
    where
      -- FIXME: honor accept header

      -- FIXME: file upload:
      --   - file deletion is the responsibility of the handler.
      --   - content type and file name are lost in digestive-functors.
      --   - remember to set upload size limit!

      bodyCheck = withRequest $ \req -> do
          tempFileState <- liftIO createInternalState
          (params, files) <- liftIO $ parseRequestBody (tempFileBackEnd tempFileState) req

          let env :: Env Identity
              env query = pure $ f (TextInput . STE.decodeUtf8) params
                              ++ f (FileInput . fileContent) files
                where
                  f :: (a -> b) -> [(SBS, a)] -> [b]
                  f g = map (g . snd)
                      . filter ((== fromPath query) . STE.decodeUtf8 . fst)

          return $ FormData env tempFileState


-- | Handle a route of type @'FormH' htm html payload@.  'formAction' is used by digestive-functors
-- as submit path for the HTML @FORM@ element.  'processor1' constructs the form, either as empty in
-- response to a @GET@, or displaying validation errors in response to a @POST@.  'processor2'
-- responds to a @POST@, handles the validated input values, and returns a new page displaying the
-- result.  Note that the renderer is monadic so that it can have effects (such as e.g. flushing a
-- message queue in the session state).
formH :: forall payload m err htm html uri.
     (Monad m, MonadError err m, ConvertibleStrings uri ST)
  => IO :~> m                     -- liftIO
  -> uri                          -- formAction
  -> Form html m payload          -- processor1
  -> (payload -> m html)          -- processor2
  -> (View html -> uri -> m html) -- renderer
  -> ServerT (FormH htm html payload) m
formH liftIO' formAction processor1 processor2 renderer = getH :<|> postH
  where
    getH :: m html
    getH = do
        v <- getForm (cs formAction) processor1
        renderer v formAction

    postH :: FormData -> m html
    postH (FormData env tmpFilesState) = do
        (v, mpayload) <- postForm (cs formAction) processor1 (\_ -> pure $ fromEnvIdentity env)
        (case mpayload of
            Just payload -> processor2 payload
            Nothing      -> renderer v formAction)
            `finally` unNat liftIO' (closeInternalState tmpFilesState)

-- | Handle a route of type @'FormH' htm html payload@ and redirect afterwards.
-- 'formAction' is used by digestive-functors as submit path for the HTML @FORM@ element.
-- 'processor1' constructs the form, either as empty in response to a @GET@, or displaying validation
-- errors in response to a @POST@.
-- 'processor2' responds to a @POST@, handles the validated input values, calculates the redirection address.
-- Note that the renderer is monadic so that it can have effects (such as e.g. flushing a
-- message queue in the session state).
formRedirectH :: forall payload m htm html uri.
     (MonadIO m, MonadError ServantErr m,
      ConvertibleStrings uri ST, ConvertibleStrings uri SBS)
  => uri                             -- ^ formAction
  -> Form html m payload             -- ^ processor1
  -> (payload -> m uri)              -- ^ processor2
  -> (View html -> uri -> m html)    -- ^ renderer
  -> ServerT (FormH htm html payload) m
formRedirectH formAction processor1 processor2 =
    formH (Nat liftIO) formAction processor1 (processor2 >=> redirect)


redirect :: (MonadServantErr err m, ConvertibleStrings uri SBS) => uri -> m a
redirect uri = throwServantErr $
    Servant.err303 { errHeaders = ("Location", cs uri) : errHeaders Servant.err303 }
