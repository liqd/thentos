{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
module Servant.Missing (FormGet, FormPost, HasForm(..)) where

import           Data.String                   (fromString)
import           GHC.TypeLits                  (KnownSymbol, symbolVal)
import           Servant                       ((:>), Proxy (..))
import           Servant.HTML.Blaze            (HTML)
import           Servant.Server.Internal       (HasServer (..),
                                                RouteResult (..),
                                                Router (WithRequest), Router,
                                                ServantErr (errBody),
                                                addBodyCheck, err400,
                                                methodRouter)

import           Control.Arrow                 (first)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as T
import           Network.HTTP.Types            (methodGet, ok200)
import           Network.Wai                   (Request, Response, requestBody,
                                                responseLBS)
import           Network.Wai.Parse             (BackEnd, parseRequestBody)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5              as H
import           Text.Digestive                (Env, Form,
                                                FormInput (TextInput), View,
                                                fromPath, getForm, postForm)

-- Combinators for digestive-functors
--
-- The 'FormGet' combinator returns the HTML for the form, and takes a unit
-- handler. The 'FormPost f v a' combinator provides an argument for the handler
-- of type 'a' in case the form can be validated; otherwise it automatically
-- returns the HTML corresponding to the error.
--
-- Note that file uploads are not supported.
--
-- > type API = "form" :> FormGet "PersonForm" Html Person
-- >       :<|> "form" :> FormPost "PersonForm" Html Person :> Post '[HTML] Person
-- >
-- > server :: Server API
-- > server = return () :<|> return
-- >
-- > data Person = Person { name :: String, age :: Int }
-- >
-- > personForm :: Monad m => Form Html m Person
-- > personForm = Person <$> "name" .: nonEmptyText
-- >                     <*> "age"  .: positiveInt
-- >   where
-- >     nonEmptyText = check "Cannot be empty" (not . Data.Text.null)
-- >                  $ text Nothing
-- >     positiveInt  = check "Must be positive" (>= 0) $ stringRead "Not a number" Nothing
-- >
-- > renderPersonForm :: View Html -> Html
-- > renderPersonForm v = ...
-- >
-- > instance HasForm "PersonForm" Html Person where
-- >    isForm _      = personForm
-- >    formView _    = renderPersonForm
-- >    formBackend _ = <save files>

data FormPost f v a
data FormGet f v a


class HasForm f v a | f -> v, f -> a where
    isForm :: Monad m => Proxy f -> Form v m a
    formView :: Proxy f -> View v -> v
    formBackend :: Proxy f -> BackEnd FilePath

instance (KnownSymbol f, HasForm f v a, Show a, H.ToMarkup v, HasServer sublayout)
         => HasServer (FormPost f v a :> sublayout) where
    type ServerT (FormPost f v a :> sub) m = a -> ServerT sub m
    route _ subserver = WithRequest $ \req ->
      route (Proxy :: Proxy sublayout) (addBodyCheck subserver $ go req)
      where
        fp = Proxy :: Proxy f
        fname = fromString $ symbolVal (Proxy :: Proxy f)
        toServantErr v = err400 { errBody = renderHtml $ H.toHtml v }
        go req = do
           (v, a) <- runFormP req fname (formBackend fp) (isForm fp)
           case a of
             Nothing -> return $ FailFatal (toServantErr $ formView fp v)
             Just a' -> return $ Route a'

instance (KnownSymbol f, HasForm f v a, H.ToMarkup v) => HasServer (FormGet f v a) where
    type ServerT (FormGet f v a) m = m ()
    route _ sub = methodRouter methodGet (Proxy :: Proxy '[HTML]) ok200 go
      where
        fname = fromString $ symbolVal (Proxy :: Proxy f)
        fp = Proxy :: Proxy f
        go = fmap (\_ -> do
            v <- getForm fname $ isForm fp
            return . H.toHtml $ formView fp v) sub


backendFormEnv :: MonadIO m => BackEnd a -> Request -> Env m
backendFormEnv be req query = do
    (q, _) <- liftIO $ parseRequestBody be req
    return $ map (TextInput . T.decodeUtf8 . snd)
           $ filter (\(x,_) -> x == fromPath query)
           $ map (first T.decodeUtf8) q

runFormP :: MonadIO m
         => Request -> Text -> BackEnd FilePath -> Form v m a -> m (View v, Maybe a)
runFormP req name backend form
    = postForm name form $ \_ -> return $ backendFormEnv backend req

