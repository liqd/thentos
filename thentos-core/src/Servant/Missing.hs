{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-| Combinators for digestive-functors

The 'FormGet' combinator takes a unit handler and returns the HTML for the form.
The 'FormPost f v a' combinator provides an argument for the handler
of type 'a' in case the form can be validated; otherwise it automatically
returns the HTML corresponding to the error.

Note that file uploads are not supported (requests containing files will crash).

> type API = "form" :> FormGet "PersonForm" Html Person
>       :<|> "form" :> FormPost "PersonForm" Html Person :> Post '[HTML] Person
>
> server :: Server API
> server = return () :<|> return
>
> data Person = Person { name :: ST, age :: Int }
>     deriving (Eq, Show, Generic, FromJSON, ToJSON)
>
> personForm :: Monad m => Form H.Html m Person
> personForm = Person <$> "name" .: nonEmptyText
>                     <*> "age"  .: positiveInt
>   where
>     nonEmptyText = check "Cannot be empty" (not . Text.null)
>                  $ text Nothing
>     positiveInt  = check "Must be positive" (> 0)
>                  $ stringRead "Not a number" Nothing
>
> renderPersonForm :: View H.Html -> ST -> H.Html
> renderPersonForm v action = form v action $ do
>     H.p $ do
>         label "name" v "Name"
>         inputText "name" v
>         errorList "name" v
>     H.p $ do
>         label "age" v "Age"
>         inputText "age" v
>         errorList "age" v
>     inputSubmit "submit"
>
> instance HasForm "test" H.Html Person where
>     formAction _  = "post_target"
>     isForm _      = personForm
>     formView _    = renderPersonForm
>     formBackend _ = error "No backend"
-}
module Servant.Missing (FormGet, FormPost, HasForm(..)) where

import           Control.Arrow                 (first)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Data.String.Conversions       (ST)
import           Data.String                   (fromString)
import qualified Data.Text.Encoding            as T
import           GHC.TypeLits                  (KnownSymbol, symbolVal)
import           Network.HTTP.Types            (methodGet, ok200)
import           Network.Wai.Parse             (BackEnd, parseRequestBody)
import           Network.Wai                   (Request)
import           Servant                       ((:>), Proxy (..))
import           Servant.API.ContentTypes      (MimeRender, mimeRender)
import           Servant.Server.Internal       (RouteResult (..), Router'(WithRequest),
                                                HasServer (..), ServantErr (errBody),
                                                addBodyCheck, err400, methodRouter)
import qualified Text.Blaze.Html5              as H
import           Text.Digestive                (Env, Form, FormInput(TextInput), View,
                                                fromPath, getForm, postForm)

data FormPost (contentTypes :: *) f v a
data FormGet (contentTypes :: *) f v a

-- FIXME: @v ~ H.Html` may be hardwired into this module in some places, and left variable in others.
class HasForm f v a | f -> v, f -> a where
    formAction :: Proxy f -> ST
    isForm :: Monad m => Proxy f -> Form v m a
    formView :: Proxy f -> View v -> ST -> v
    formBackend :: Proxy f -> BackEnd FilePath

instance (MimeRender ct H.Html, KnownSymbol f, HasForm f v a, H.ToMarkup v, HasServer sublayout)
         => HasServer (FormPost ct f v a :> sublayout) where
    type ServerT (FormPost ct f v a :> sub) m = a -> ServerT sub m
    route _ subserver = WithRequest $ \req ->
      route (Proxy :: Proxy sublayout) (addBodyCheck subserver $ go req)
      where
        fp = Proxy :: Proxy f
        fname = fromString $ symbolVal fp
        toServantErr v = err400 { errBody = mimeRender (Proxy :: Proxy ct) $ H.toHtml v }
        go req = do
           (v, a) <- runFormP req fname (formBackend fp) (isForm fp)
           case a of
             Nothing -> return . FailFatal . toServantErr $ formView fp v (formAction fp)
             Just a' -> return $ Route a'

instance (MimeRender ct H.Html, KnownSymbol f, HasForm f v a, H.ToMarkup v)
      => HasServer (FormGet ct f v a) where
    type ServerT (FormGet ct f v a) m = m ()
    route _ sub = methodRouter methodGet (Proxy :: Proxy '[ct]) ok200 (go <$> sub)
      where
        fname = fromString $ symbolVal (Proxy :: Proxy f)
        fp = Proxy :: Proxy f
        go _ = do
            v <- getForm fname $ isForm fp
            return . H.toHtml $ formView fp v (formAction fp)


backendFormEnv :: MonadIO m => BackEnd a -> Request -> Env m
backendFormEnv be req query = do
    q <- liftIO (parseRequestBody be req) >>=
          \case (q, []) -> return q
                (_, _:_) -> error "servant-digestive-functors: file upload not implemented."
    return $ map (TextInput . T.decodeUtf8 . snd)
           $ filter ((== fromPath query) . fst)
           $ map (first T.decodeUtf8) q

runFormP :: MonadIO m
         => Request -> ST -> BackEnd FilePath -> Form v m a -> m (View v, Maybe a)
runFormP req name backend form
    = postForm name form $ \_ -> return $ backendFormEnv backend req
