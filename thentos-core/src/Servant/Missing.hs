-- FIXME: create a package servant-digestive-functors and
--        choose a different module name.
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.Missing
  (FormH
  ,FormReqBody
  ,FormData, getFormDataEnv, releaseFormTempFiles
  ,formH
  ,formRedirectH
  ,fromEnvIdentity
  ,redirect) where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Except.Missing (finally)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (InternalState, createInternalState, closeInternalState)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (ST, SBS, ConvertibleStrings, cs)
import Network.Wai.Parse (fileContent, parseRequestBody, tempFileBackEnd)
import Network.Wai (Request)
import Servant ((:<|>)((:<|>)), (:>), ServerT, (:~>)(Nat), unNat)
import Servant.Server (ServantErr(..))
import Servant.Server.Internal (HasServer, Router'(WithRequest), RouteResult(Route),
                                route, addBodyCheck)
import Text.Digestive (Env, Form, FormInput(TextInput, FileInput), View, fromPath, getForm, postForm)

import qualified Servant
import qualified Data.Text.Encoding as STE

type FormH htm html payload =
         Servant.Get '[htm] html
    :<|> FormReqBody :> Servant.Post '[htm] html

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

instance (HasServer sublayout) => HasServer (FormReqBody :> sublayout) where
  type ServerT (FormReqBody :> sublayout) m = FormData -> ServerT sublayout m

  route Proxy subserver = WithRequest $ \request ->
      route (Proxy :: Proxy sublayout) (addBodyCheck subserver (bodyCheck request))
    where
      -- FIXME: honor accept header

      -- FIXME: file upload:
      --   - file deletion is the responsibility of the handler.
      --   - content type and file name are lost in digestive-functors.
      --   - remember to set upload size limit!

      bodyCheck :: Request -> IO (RouteResult FormData)
      bodyCheck req = do
          tempFileState <- createInternalState
          (params, files) <- parseRequestBody (tempFileBackEnd tempFileState) req

          let env :: Env Identity
              env query = pure $ f (TextInput . STE.decodeUtf8) params
                              ++ f (FileInput . fileContent) files
                where
                  f :: (a -> b) -> [(SBS, a)] -> [b]
                  f g = map (g . snd)
                      . filter ((== fromPath query) . STE.decodeUtf8 . fst)

          return $ Route (FormData env tempFileState)


-- | Handle a route of type @'FormH' htm html payload@.  'formAction' is used by digestive-functors
-- as submit path for the HTML @FORM@ element.  'processor1' constructs the form, either as empty in
-- response to a @GET@, or displaying validation errors in response to a @POST@.  'processor2'
-- responds to a @POST@, handles the validated input values, and returns a new page displaying the
-- result.  Note that the renderer is monadic so that it can have effects (such as e.g. flushing a
-- message queue in the session state).
formH :: forall payload m err htm html uri.
     (Monad m, MonadError err m, ConvertibleStrings uri ST)
  => IO :~> m                     -- ^ liftIO
  -> uri                          -- ^ formAction
  -> Form html m payload          -- ^ processor1
  -> (payload -> m html)          -- ^ processor2
  -> (View html -> uri -> m html) -- ^ renderer
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
formRedirectH formAction processor1 processor2 renderer =
    formH (Nat liftIO) formAction processor1 (\p -> processor2 p >>= redirect) renderer


redirect :: (MonadError ServantErr m, ConvertibleStrings uri SBS) => uri -> m a
redirect uri = throwError $ Servant.err303 { errHeaders = ("Location", cs uri) : errHeaders Servant.err303 }
