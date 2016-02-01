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
  ,formH) where

import Control.Monad.Identity (Identity, runIdentity)
import Data.Bifunctor (first)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (ST)
import Network.Wai.Parse (Param, parseRequestBody, lbsBackEnd)
import Network.Wai (Request)
import Servant ((:<|>)((:<|>)), (:>), ServerT)
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


-- FIXME: parts of this comment are specific to thentos
-- | There are two form processor functions that are combined in the execution: the
-- digestive-functors part that composes the form payload or a list of errors to be sent back to the
-- browser, and the part that has effects on system and session state.  The first is factored out so
-- it can live in the Safe "Pages" module.  The renderer needs to be an action so it can e.g. flush
-- and render the 'FrontendMsg' queue.
--
formH :: forall payload m htm html.
     Monad m
  => ST                           -- ^ formAction
  -> Form html m payload          -- ^ processor I
  -> (payload -> m html)          -- ^ processor II
  -> (View html -> ST -> m html)  -- ^ renderer
  -> ServerT (FormH htm html payload) m
formH formAction processor1 processor2 renderer = getH :<|> postH
  where
    getH = do
        v <- getForm formAction processor1
        renderer v formAction

    postH :: Env Identity -> m html
    postH env = postForm formAction processor1 (\_ -> return $ return . runIdentity . env) >>=
        \case (_,              Just payload) -> processor2 payload
              (v :: View html, Nothing)      -> renderer v formAction
