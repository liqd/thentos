{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE DeriveDataTypeable                       #-}
{-# LANGUAGE ExistentialQuantification                #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE InstanceSigs                             #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TupleSections                            #-}
{-# LANGUAGE TypeFamilies                             #-}
{-# LANGUAGE TypeOperators                            #-}
{-# LANGUAGE TypeSynonymInstances                     #-}
{-# LANGUAGE UndecidableInstances                     #-}

{-# OPTIONS  #-}

module Thentos.Backend.Core
where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar (MVar)
import Control.Exception (Exception, assert, fromException, toException)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Either (EitherT(EitherT), runEitherT)
import Control.Monad.Trans.Reader (runReaderT)
import Crypto.Random (SystemRNG)
import Data.CaseInsensitive (CI, mk, foldCase, foldedCase)
import Data.Char (isUpper)
import Data.Configifier ((>>.))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (cs)
import Data.String.Conversions (SBS, ST)
import Data.String (fromString)
import Data.Text.Encoding (decodeUtf8')
import Data.Thyme.Time ()
import Data.Typeable (Typeable)
import Network.HTTP.Types (Header)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (runSettings, setHost, setPort, defaultSettings)
import Network.Wai (ResponseReceived, Request, requestHeaders)
import Servant.API ((:<|>)((:<|>)))
import Servant.Server (HasServer, Server, route)
import System.Log.Logger (Priority(CRITICAL))

import qualified Data.ByteString.Char8 as SBS

import System.Log.Missing (logger)
import Thentos.DB
import Thentos.Api
import Thentos.Config
import Thentos.Types


-- * types

type RestAction      = Action (MVar SystemRNG)
type RestActionState = ActionState (MVar SystemRNG)
type RestActionRaw   = EitherT RestError IO
type RestError       = (Int, String)


-- * error rendering

-- | Render errors for servant.  (The servant error type will
-- hopefully change in the future.)
class ThentosErrorServant e where
    renderErrorServant :: e -> (Int, String)

data SomeThentosErrorServant = forall e . (ThentosErrorServant e, ThentosError e) => SomeThentosErrorServant e
  deriving Typeable

instance Show SomeThentosErrorServant where
    showsPrec p (SomeThentosErrorServant e) = showsPrec p e

instance Exception SomeThentosErrorServant where
    toException = thentosErrorToException
    fromException = thentosErrorFromException

instance ThentosErrorServant SomeThentosErrorServant where
    renderErrorServant (SomeThentosErrorServant e) = renderErrorServant e


-- | Handle all errors in 'SomeThentosErrorServant' with the
-- resp. rendering, and handle all other errors in 'SomeThentosError'
-- with 500 responses.
--
-- (It would be nice to have a guarantee from the type checker that
-- this will never happen, but the DB transactions all return
-- 'SomeThentosError' at the point of writing this.)
renderError :: (MonadIO m, ThentosError e) => e -> m (Int, String)
renderError e = case fromException $ toException e of
        Just (e' :: SomeThentosErrorServant)-> return $ renderErrorServant e'

@@

-- FIXME: this is still broken (remove syntax error in line above this
-- comment and run the test suite!)  i can't hope that where it is
-- possible due to class constraints, 'SomeThentosErrorServant' will
-- somehow magically wrap itself around the error formerly wrapped in
-- 'SomeThentosError' (even if the latter wrap somehow magically
-- disappears).  need another solution!


        Nothing -> do
            logger CRITICAL $ "uncaught exception in servant: " ++ show e
            return (500, "internal error.")


instance ThentosErrorServant NoSuchUser where
    renderErrorServant NoSuchUser = (404, "user not found")

instance ThentosErrorServant NoSuchPendingUserConfirmation where
    renderErrorServant NoSuchPendingUserConfirmation = (404, "unconfirmed user not found")

instance ThentosErrorServant NoSuchService where
    renderErrorServant NoSuchService = (404, "service not found")

instance ThentosErrorServant NoSuchSession where
    renderErrorServant NoSuchSession = (404, "session not found")

instance ThentosErrorServant OperationNotPossibleInServiceSession where
    renderErrorServant OperationNotPossibleInServiceSession = (404, "operation not possible in service session")

instance ThentosErrorServant UserEmailAlreadyExists where
    renderErrorServant UserEmailAlreadyExists = (403, "email already in use")

instance ThentosErrorServant UserNameAlreadyExists where
    renderErrorServant UserNameAlreadyExists = (403, "user name already in use")

instance ThentosErrorServant PermissionDenied where
    renderErrorServant (PermissionDenied _ _ _) = (401, "unauthorized")

instance ThentosErrorServant BadCredentials where
    renderErrorServant BadCredentials = (401, "unauthorized")

instance ThentosErrorServant NoSuchResetToken where
    renderErrorServant NoSuchResetToken = (404, "no such password reset token")

instance ThentosErrorServant MalformedConfirmationToken where
    renderErrorServant (MalformedConfirmationToken path) = (400, "malformed confirmation token: " ++ show path)


-- * turning the handler monad into 'Action'

-- | This is a work-around: The 'Server' type family terminates in
-- 'RestActionRaw' on all methods.  'PushActionC' instances transform
-- handlers implemented in a monad stack we want (providing acid
-- state, clearance info, random generator, ... in a reader) into the
-- handlers in 'RestActionRaw'.  (Also, translate 'DbError' to
-- 'RestError'.)
class PushActionC a where
    type PushActionSubRoute a
    pushAction :: RestActionState -> PushActionSubRoute a -> a

instance (PushActionC b) => PushActionC (a -> b) where
    type PushActionSubRoute (a -> b) = a -> PushActionSubRoute b
    pushAction clearance f = pushAction clearance . f

instance (PushActionC a, PushActionC b) => PushActionC (a :<|> b) where
    type PushActionSubRoute (a :<|> b) = PushActionSubRoute a :<|> PushActionSubRoute b
    pushAction clearance (a :<|> b) = pushAction clearance a :<|> pushAction clearance b

instance PushActionC (RestActionRaw a) where
    type PushActionSubRoute (RestActionRaw a) = RestAction a
    pushAction restState restAction = fmapLTM renderError $ runReaderT restAction restState

-- | For handling 'Raw'.  (The 'Application' type has been stripped of
-- its arguments by the time the compiler will find this instance.)
--
-- FIXME: in order to do error handling here, which would be nice, we
-- would have to instantiate @((Response -> IO ResponseReceived) -> IO
-- ResponseReceived)@, which conflicts with the @a -> b@ instance
-- above.  a solution for this is to wrap 'Application' in 'Raw' into
-- a transparent newtype that can be instantiated here instead of the
-- function type.  for now, just crash in case of errors.
instance PushActionC (IO ResponseReceived) where
    type PushActionSubRoute (IO ResponseReceived) = RestAction ResponseReceived
    pushAction restState restAction = (either crash id <$>) . runEitherT $ runReaderT restAction restState
      where
        crash x = assert False $ error $ "[PushActionC for Raw] somebody threw an error, but we can't handle those yet: "
                    ++ show x

-- | Like 'fmapLT' from "Data.EitherR", but with the update of the
-- left value constructed in an impure action.
fmapLTM :: (Monad m, Functor m) => (a -> m a') -> EitherT a m b -> EitherT a' m b
fmapLTM trans e = EitherT $ do
    result <- runEitherT e
    case result of
        Right r -> return $ Right r
        Left l -> Left <$> trans l


-- * header invariant

data ThentosHeaderName =
    ThentosHeaderSession
  | ThentosHeaderService
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable)

renderThentosHeaderName :: ThentosHeaderName -> CI SBS
renderThentosHeaderName x = case splitAt (SBS.length "ThentosHeader") (show x) of
    ("ThentosHeader", s) -> mk . SBS.pack $ "X-Thentos" ++ dashify s
    bad -> error $ "renderThentosHeaderName: bad prefix (left side) in " ++ show bad
  where
    dashify ""    = ""
    dashify (h:t) = if isUpper h
        then '-' : h : dashify t
        else       h : dashify t

assertSoundHeadersHelper :: [Header] -> Bool
assertSoundHeadersHelper = null . badHeadersHelper

badHeadersHelper :: [Header] -> [Header]
badHeadersHelper = filter g . filter f
  where
    f (k, _) = foldCase "X-Thentos-" `SBS.isPrefixOf` foldedCase k
    g (k, _) = not $ k `elem` map renderThentosHeaderName [minBound..]

lookupRequestHeader :: Request -> ThentosHeaderName -> Maybe ST
lookupRequestHeader req key =
          lookup (renderThentosHeaderName key) (requestHeaders req)
      >>= either (const Nothing) Just . decodeUtf8'

data ThentosAssertHeaders layout = ThentosAssertHeaders layout

instance ( HasServer sublayout ) => HasServer (ThentosAssertHeaders sublayout)
  where
    type Server (ThentosAssertHeaders sublayout) = Server sublayout

    route Proxy subserver request respond =
        case badHeadersHelper (requestHeaders request) of
            []  -> route (Proxy :: Proxy sublayout) subserver request respond
            bad -> error $ "ThentosAssertHeaders: " ++ show bad
                  -- FIXME: wait for better error support in servant?


-- * warp

runWarpWithCfg :: HttpConfig -> Application -> IO ()
runWarpWithCfg cfg = runSettings settings
  where
    settings = setPort (cfg >>. (Proxy :: Proxy '["bind_port"]))
             . setHost (fromString . cs $ cfg >>. (Proxy :: Proxy '["bind_host"]))
             $ defaultSettings
