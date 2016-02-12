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
import Data.Bifunctor (first)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (ST, cs)
import Network.Wai.Parse (Param, parseRequestBody, lbsBackEnd)
import Network.Wai (Request)
import Servant ((:<|>)((:<|>)), (:>), ServerT)
import Servant.Server (ServantErr(..))
import Servant.Server.Internal (HasServer, Router'(WithRequest), RouteResult(Route),
                                route, addBodyCheck)
import Text.Digestive (Env, Form, FormInput(TextInput), View, fromPath, getForm, postForm)

import qualified Servant
import qualified Data.Text.Encoding as STE

type FormH htm html payload =
         Servant.Get '[htm] html
    :<|> FormReqBody :> Servant.Post '[htm] html

data FormReqBody

instance (HasServer sublayout) => HasServer (FormReqBody :> sublayout) where
  type ServerT (FormReqBody :> sublayout) m = Env Identity -> ServerT sublayout m

  route Proxy subserver = WithRequest $ \request ->
      route (Proxy :: Proxy sublayout) (addBodyCheck subserver (bodyCheck request))
    where
      -- FIXME: honor accept header
      -- FIXME: file upload.  shouldn't be hard!
      bodyCheck :: Request -> IO (RouteResult (Env Identity))
      bodyCheck req = do
          q :: [Param]
              <- parseRequestBody lbsBackEnd req >>=
                  \case (q, []) -> return q
                        (_, _:_) -> error "servant-digestive-functors: file upload not implemented!"

          let env :: Env Identity
              env query = return
                        . map (TextInput . STE.decodeUtf8 . snd)
                        . filter ((== fromPath query) . fst)
                        . map (first STE.decodeUtf8)
                        $ q

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
    postH env = postForm formAction processor1 (\_ -> return $ return . runIdentity . env) >>=
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
    postH env = postForm formAction processor1 (\_ -> return $ return . runIdentity . env) >>=
        \case (_,              Just payload) -> processor2 payload >>= redirect
              (v :: View html, Nothing)      -> renderer v formAction

    redirect uri = throwError $ Servant.err303 { errHeaders = ("Location", cs uri) : errHeaders Servant.err303 }
