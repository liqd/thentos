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
  ,formH
  ,formRedirectH) where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans.Resource (InternalState, createInternalState)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (ST, cs)
import Network.Wai.Parse (Param, File, fileContent, parseRequestBody, tempFileBackEnd)
import Network.Wai (Request)
import Servant ((:<|>)((:<|>)), (:>), ServerT)
import Servant.Server (ServantErr(..))
import Servant.Server.Internal (HasServer, Router'(WithRequest), RouteResult(Route),
                                route, addBodyCheck)
import System.IO.Unsafe (unsafePerformIO)
import Text.Digestive (Env, Form, FormInput(TextInput, FileInput), View, fromPath, getForm, postForm)

import qualified Servant
import qualified Data.Text.Encoding as STE

type FormH htm html payload =
         Servant.Get '[htm] html
    :<|> FormReqBody :> Servant.Post '[htm] html

data FormReqBody

fromEnvIdentity :: Applicative m => Env Identity -> Env m
fromEnvIdentity env = pure . runIdentity . env

-- | FIXME: not sure this is legal.  do we need to make the IORef thread safe?  by wrapping it into
-- an STRef?  but it seems like we are using it exactly like it is intended to.
tempFileState :: InternalState
tempFileState = unsafePerformIO createInternalState
{-# NOINLINE tempFileState #-}

instance (HasServer sublayout) => HasServer (FormReqBody :> sublayout) where
  type ServerT (FormReqBody :> sublayout) m = Env Identity -> ServerT sublayout m

  route Proxy subserver = WithRequest $ \request ->
      route (Proxy :: Proxy sublayout) (addBodyCheck subserver (bodyCheck request))
    where
      -- FIXME: honor accept header

      -- FIXME: file upload:
      --   - file deletion is the responsibility of the handler.
      --   - content type and file name are lost in digestive-functors.
      --   - remember to set upload size limit!

      bodyCheck :: Request -> IO (RouteResult (Env Identity))
      bodyCheck req = do
          (params, files) :: ([Param], [File FilePath])
              <- parseRequestBody (tempFileBackEnd tempFileState) req

          let env :: Env Identity
              env query = pure $ ps ++ fs
                where
                  ps = map (TextInput . STE.decodeUtf8 . snd)
                     . filter ((== fromPath query) . STE.decodeUtf8 . fst)
                     $ params

                  fs = map (FileInput . fileContent . snd)
                     . filter ((== fromPath query) . STE.decodeUtf8 . fst)
                     $ files

          return $ Route env


-- | Handle a route of type @'FormH' htm html payload@.  'formAction' is used by digestive-functors
-- as submit path for the HTML @FORM@ element.  'processor1' constructs the form, either as empty in
-- response to a @GET@, or displaying validation errors in response to a @POST@.  'processor2'
-- responds to a @POST@, handles the validated input values, and returns a new page displaying the
-- result.  Note that the renderer is monadic so that it can have effects (such as e.g. flushing a
-- message queue in the session state).
formH :: forall payload m htm html.
     Monad m
  => ST                           -- ^ formAction
  -> Form html m payload          -- ^ processor1
  -> (payload -> m html)          -- ^ processor2
  -> (View html -> ST -> m html)  -- ^ renderer
  -> ServerT (FormH htm html payload) m
formH formAction processor1 processor2 renderer = getH :<|> postH
  where
    getH :: m html
    getH = do
        v <- getForm formAction processor1
        renderer v formAction

    postH :: Env Identity -> m html
    postH env = postForm formAction processor1 (\_ -> pure $ fromEnvIdentity env) >>=
        \case (_,              Just payload) -> processor2 payload
              (v :: View html, Nothing)      -> renderer v formAction

-- | Handle a route of type @'FormH' htm html payload@ and redirect afterwards.
-- 'formAction' is used by digestive-functors as submit path for the HTML @FORM@ element.
-- 'processor1' constructs the form, either as empty in response to a @GET@, or displaying validation
-- errors in response to a @POST@.
-- 'processor2' responds to a @POST@, handles the validated input values, calculates the redirection address.
-- Note that the renderer is monadic so that it can have effects (such as e.g. flushing a
-- message queue in the session state).
--
-- FIXME: see github issue #494.
formRedirectH :: forall payload m htm html.
     (Monad m, MonadError ServantErr m)
  => ST                              -- ^ formAction
  -> Form html m payload             -- ^ processor1
  -> (payload -> m ST)               -- ^ processor2
  -> (View html -> ST -> m html)     -- ^ renderer
  -> ServerT (FormH htm html payload) m
formRedirectH formAction processor1 processor2 renderer = getH :<|> postH
  where
    getH = do
        v <- getForm formAction processor1
        renderer v formAction

    postH :: Env Identity -> m html
    postH env = postForm formAction processor1 (\_ -> pure $ fromEnvIdentity env) >>=
        \case (_,              Just payload) -> processor2 payload >>= redirect
              (v :: View html, Nothing)      -> renderer v formAction

    redirect uri = throwError $ Servant.err303 { errHeaders = ("Location", cs uri) : errHeaders Servant.err303 }
