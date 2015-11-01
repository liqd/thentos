{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-binds #-}

module Servant.MissingSpec (spec) where

import           Control.Concurrent.MVar       (MVar, newMVar, readMVar, modifyMVar)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad                 (replicateM_)
import           Data.ByteString.Lazy.Char8    (unpack)
import           Data.Monoid                   ((<>))
import           Data.String.Conversions       (ConvertibleStrings, ST, cs)
import qualified Data.Text                     as Text
import           GHC.Generics                  (Generic)
import           Network.Wai.Test              (simpleBody)
import           Servant                       (Proxy(Proxy), Get, Post, JSON, Server,
                                                (:>), (:<|>)((:<|>)), serve)
import           Servant.HTML.Blaze            (HTML)
import           System.IO.Unsafe              (unsafePerformIO)
import           Test.Hspec                    (Spec, describe, context, it, shouldBe, shouldContain)
import           Test.Hspec.Wai                (get, post, postHtmlForm, shouldRespondWith, with)
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Text.Digestive.Blaze.Html5    (form, inputSubmit, inputText, label, errorList)
import           Text.Digestive                (Form, View, check, text, (.:), getForm, stringRead)

import           Servant.Missing
import           Thentos.Test.Core


spec :: Spec
spec = describe "Servant.Missing" $ do
    formSpec
    formSpecWithAppState


-- without app state

formSpec :: Spec
formSpec = describe "Forms without state" $ with (return $ serve api server) $ do
    context "FormGet" $ do
        it "returns the Html from 'formView'" $ do
            let formN = "test"
                formP = Proxy :: Proxy "test"
            v <- getForm formN personForm
            r <- get "form"

            liftIO $ simpleBody r `shouldBe` renderHtml (renderPersonForm v (formAction formP))

        it "responds with 200" $ do
            get "form" `shouldRespondWith` 200

    context "FormPost" $ do
        it "responds with 400 if the form is invalid" $ do
            postHtmlForm "form" [] `shouldRespondWith` 400
            postHtmlForm "form" [("test.name", "aname"), ("test.age", "-1")]
                `shouldRespondWith` 400

        it "returns the validation errors" $ do
            r1 <- postHtmlForm "form" [("test.name", ""), ("test.age", "1")]
            liftIO $ unpack (simpleBody r1) `shouldContain` "Cannot be empty"
            r2 <- postHtmlForm "form" [("test.name", "aname"), ("test.age", "-1")]
            liftIO $ unpack (simpleBody r2) `shouldContain` "Must be positive"

        it "accepts valid forms" $ do
            postHtmlForm "form" [("test.name", "aname"), ("test.age", "1")]
                `shouldRespondWith` 201


type API = "form" :> (FormGet HTML "test" H.Html Person ()
      :<|> FormPost HTML "test" H.Html Person :> Post '[HTML] Person)

api :: Proxy API
api = Proxy

server :: Server API
server = return () :<|> (\(Right page) -> return page)

data Person = Person { name :: ST, age :: Int }
    deriving (Eq, Show, Generic)

instance H.ToMarkup Person where
    toMarkup (Person n a) = H.div $ H.text n >> H.string (show a)

personForm :: Monad m => Form H.Html m Person
personForm = Person <$> "name" .: nonEmptyText
                    <*> "age"  .: positiveInt
  where
    nonEmptyText = check "Cannot be empty" (not . Text.null)
                 $ text Nothing
    positiveInt  = check "Must be positive" (> 0)
                 $ stringRead "Not a number" Nothing

renderPersonForm :: View H.Html -> ST -> H.Html
renderPersonForm v action = form v action $ do
    H.p $ do
        label "name" v "Name"
        inputText "name" v
        errorList "name" v
    H.p $ do
        label "age" v "Age"
        inputText "age" v
        errorList "age" v
    inputSubmit "submit"

instance HasForm "test" H.Html Person () where
    formAction _  = "post_target"
    isForm _      = personForm
    formView _    = \_ -> renderPersonForm
    formBackend _ = error "No backend"


-- with app state

formSpecWithAppState :: Spec
formSpecWithAppState = describe "Forms with state" $ with (clearGS >> return (serve apiWas serverWas)) $ do
    context "state api" $ do
        it "has working inc, get endpoints" $ do
            get "state/get" `shouldRespondWith` "0"
            post "state/inc" ""
            get "state/get" `shouldRespondWith` "1"

    context "FormGet" $ do
        let formN = "testWas"
            formP = Proxy :: Proxy "testWas"

            go state = do
                v <- getForm formN personForm
                r <- get "form"

                let is = simpleBody r
                    should = renderHtml (renderPersonFormWas (Just state) v (formAction formP))

                liftIO $ diffXml is should `shouldBe` []

        it "handles state" $ do
            go 0
            replicateM_ 3 $ post "state/inc" ""
            go 3

    context "FormPost" $ do
        let go i = do
            r <- postHtmlForm "form" [("test.name", "mr. young"), ("test.age", "1")]
            liftIO $ cs (simpleBody r) `shouldContain` ("<p>Just " ++ show i ++ "</p>")

        it "handles state correctly" $ do
            go 0
            replicateM_ 4 $ post "state/inc" ""
            go 4


type APIWAS =
       "form" :> (
              FormGet HTML "testWas" H.Html Person Int
         :<|> FormPost HTML "testWas" H.Html Person :> Post '[HTML] Person
         )
  :<|> "state" :> (
              "get" :> Get '[HTML] Int
         :<|> "inc" :> Post '[HTML] ()
         )

serverWas :: Server APIWAS
serverWas = (getGS :<|> postHandler) :<|> (getGS :<|> incGS)
  where
    postHandler Nothing = Left <$> getGS
    postHandler (Just page) = return $ Right page

renderPersonFormWas :: Maybe Int -> View H.Html -> ST -> H.Html
renderPersonFormWas state v action = form v action $ do
    H.p $ H.string (show state)
    H.p $ do
        label "name" v "Name"
        inputText "name" v
        errorList "name" v
    H.p $ do
        label "age" v "Age"
        inputText "age" v
        errorList "age" v
    inputSubmit "submit"

instance HasForm "testWas" H.Html Person Int where
    formAction _  = "post_target"
    isForm _      = personForm
    formView _    = renderPersonFormWas
    formBackend _ = error "No backend"

globalState :: MVar Int
globalState = unsafePerformIO $ newMVar 0
{-# NOINLINE globalState #-}

clearGS :: MonadIO m => m ()
clearGS = liftIO $ modifyMVar globalState (\_ -> return (0, ()))

incGS :: MonadIO m => m ()
incGS = liftIO $ modifyMVar globalState (\s -> return (s+1, ()))

getGS :: MonadIO m => m Int
getGS = liftIO $ readMVar globalState

apiWas :: Proxy APIWAS
apiWas = Proxy


-- * frivolous hacks

-- | Hacky.  consider using
-- https://hackage.haskell.org/package/Diff-0.3.2/docs/Data-Algorithm-Diff.html
--
-- ...  or something like this:
-- >>> data Diff a = DiffIn a | DiffOut a
-- >>>   deriving (Eq, Show)
diff :: ConvertibleStrings a String => a -> a -> [String]
diff xs ys = f 0 (lines $ cs xs) (lines $ cs ys)
  where
    f _ []       []     = []
    f i (x:xs)   []     = (showIn i x) : f (i+1) xs []
    f i []       (y:ys) = (showOut i y) : f (i+1) [] ys
    f i (x:xs)   (y:ys) =
        (if x == y then [] else [showIn i x, showOut i y]) ++
        f (i+1) xs ys

    showIn i x = showAligned i <> " < " <> x
    showOut i y = showAligned i <> " > " <> y
    showAligned = reverse . take 5 . reverse . ("     " <>) . show

-- | Convenience hack for 'diff'
diffXml :: ConvertibleStrings a String => a -> a -> [String]
diffXml xs ys = diff (concat . map f . cs $ xs) (concat . map f . cs $ ys)
  where
    f '>' = ">\n"
    f c = [c]

renderDiff :: [String] -> IO ()
renderDiff = putStr . ("\n" <>) . (<> "\n") . unlines
