{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.Missing where

import Servant
import Servant.HTML.Blaze (HTML)
import Servant.Server.Internal

import Control.Arrow (first)
import Control.Monad.IO.Class
import Text.Digestive
import Network.Wai (Request, Response, responseLBS, requestBody)
import Network.Wai.Parse (BackEnd, parseRequestBody)
import Network.HTTP.Types (ok200, methodGet)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Text.Encoding as T
import Network.Wai.Digestive

import Debug.Trace

-- Combinators for digestive-functors
--
-- The 'FormGet' combinator returns the HTML for the form, and takes a unit
-- handler. The 'FormPost f v a' combinator provides an argument for the handler
-- of type 'a' in case the form can be validated; otherwise it automatically
-- returns the HTML corresponding to the error.
--
-- > type API = "form" :> FormGet "PersonForm" Html Person
-- >       :<|> "form" :> FormPost "PersonForm" Html Person :> Post '[HTML] Person
-- >
-- > server :: Server API
-- > server = () :<|> id
-- >
-- > data Person = Person { name :: String, age :: Int }
-- >
-- > personForm :: Monad m => Form Html m Person
-- > personForm = Person <$> "name" .: nonEmptyText
-- >                     <*> "age"  .: positiveInt
-- >   where
-- >     nonEmptyText = check "Cannot be empty" (not . Data.Text.null)
-- >                  $ text Nothing
-- >     positiveInt  = check "Must be postive" (>= 0) $ stringRead Nothing
-- >
-- > renderPersonForm :: View Html -> Html
-- > renderPersonForm v = ...
-- >
-- > instance HasForm "PersonForm" Html Person where
-- >    isForm _   = personForm
-- >    formView _ = renderPersonForm

data FormPost f v a
data FormGet f v a

toServantErr :: H.ToMarkup v => v -> ServantErr
toServantErr v = err400 { errBody = renderHtml $ H.toHtml v }

render :: H.Html -> RouteResult Response
render x = Route $ responseLBS ok200 [] $ renderHtml x

class HasForm f v a | f -> v, f -> a where
    isForm :: Monad m => Proxy f -> Form v m a
    formView :: Proxy f -> View v -> v
    formBackend :: Proxy f -> BackEnd FilePath

instance (HasForm f v a, Show a, H.ToMarkup v, HasServer sublayout)
         => HasServer (FormPost f v a :> sublayout) where
    type ServerT (FormPost f v a :> sub) m = a -> ServerT sub m
    route _ subserver = WithRequest $ \req ->
      route (Proxy :: Proxy sublayout) (addBodyCheck subserver $ go req)
      where
        fp = Proxy :: Proxy f
        go req = do
           (v, a) <- runFormP req "test" (formBackend fp) (isForm fp)
           case a of
             Nothing -> return $ FailFatal (toServantErr $ formView fp v)
             Just a' -> traceShow a $ return $ Route a'

instance (HasForm f v a, H.ToMarkup v) => HasServer (FormGet f v a) where
    type ServerT (FormGet f v a) m = m ()
    route _ sub = methodRouter methodGet (Proxy :: Proxy '[HTML]) ok200 go
      where
        fp = Proxy :: Proxy f
        go = fmap (\_ -> do
            v <- runFormG "test" $ isForm fp
            return . H.toHtml $ formView fp v) sub

bfe :: MonadIO m => BackEnd a -> Request -> Env m
bfe be req query = do
    (q, _) <- liftIO $ parseRequestBody be req
    liftIO $ putStrLn ("ReqBody " ++ show q)
    liftIO $ putStrLn ("query " ++ show (fromPath query))
    let q' = map (first T.decodeUtf8) q
    let res = map (TextInput . T.decodeUtf8 . snd) $ filter (\(x,_) -> x == fromPath query) q'
    liftIO $ putStrLn ("Res " ++ show res)
    return $ res

runFormG :: Monad m => Text -> Form v m a -> m (View v)
runFormG = getForm

runFormP :: MonadIO m
    => Request -> Text -> BackEnd FilePath -> Form v m a -> m (View v, Maybe a)
runFormP req name backend form = postForm name form $ \x -> case x of
    MultiPart  -> trace "here1" $ bodyFormEnv backend req
    UrlEncoded -> return $ bfe backend req
