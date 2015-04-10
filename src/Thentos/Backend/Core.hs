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

import Control.Applicative ((<$>), (<|>))
import Control.Concurrent.MVar (MVar)
import Control.Exception (Exception, assert)
import Control.Monad.IO.Class (MonadIO, liftIO)
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

import qualified Data.ByteString.Char8 as SBS

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
class ThentosError e => ThentosErrorServant e where

data SomeThentosErrorServant =
    forall e . ThentosErrorServant e => SomeThentosErrorServant e
  deriving Typeable

instance Show SomeThentosErrorServant where
    showsPrec p (SomeThentosErrorServant e) = showsPrec p e

instance Exception SomeThentosErrorServant
instance ThentosError SomeThentosErrorServant
instance ThentosErrorServant SomeThentosErrorServant

renderError :: (MonadIO m, ThentosError e) => e -> m (Int, String)
renderError = catchServant <|> catchInternal

catchServant :: (MonadIO m) => e -> m (Int, String)
catchServant = _

catchInternal :: (MonadIO m) => e -> m (Int, String)
catchInternal = _


--     thentosErrorServantShow :: MonadIO m => e -> m (Int, String)


{-
instance ThentosErrorShowServant NoSuchUser where
    showThentosError NoSuchUser = return (404, "user not found")

instance ThentosErrorShowServant NoSuchPendingUserConfirmation where
    showThentosError NoSuchPendingUserConfirmation = return (404, "unconfirmed user not found")

instance ThentosErrorShowServant MalformedConfirmationToken where
    showThentosError (MalformedConfirmationToken path) = return (400, "malformed confirmation token: " ++ show path)

instance ThentosErrorShowServant NoSuchService where
    showThentosError NoSuchService = return (404, "service not found")

instance ThentosErrorShowServant NoSuchSession where
    showThentosError NoSuchSession = return (404, "session not found")

instance ThentosErrorShowServant OperationNotPossibleInServiceSession where
    showThentosError OperationNotPossibleInServiceSession = return (404, "operation not possible in service session")

instance ThentosErrorShowServant ServiceAlreadyExists where
    showThentosError ServiceAlreadyExists = return (403, "service already exists")

instance ThentosErrorShowServant UserEmailAlreadyExists where
    showThentosError UserEmailAlreadyExists = return (403, "email already in use")

instance ThentosErrorShowServant UserNameAlreadyExists where
    showThentosError UserNameAlreadyExists = return (403, "user name already in use")

instance ThentosErrorShowServant PermissionDenied where
    showThentosError e@(PermissionDenied _ _ _) = logger INFO (show e) >> return (401, "unauthorized")

instance ThentosErrorShowServant BadCredentials where
    showThentosError e@BadCredentials = logger INFO (show e) >> return (401, "unauthorized")

instance ThentosErrorShowServant BadAuthenticationHeaders where
    showThentosError BadAuthenticationHeaders = return (400, "bad authentication headers")

instance ThentosErrorShowServant ProxyNotAvailable where
    showThentosError ProxyNotAvailable = return (404, "proxying not activated")

instance ThentosErrorShowServant MissingServiceHeader where
    showThentosError MissingServiceHeader = return (404, "headers do not contain service id")

instance ThentosErrorShowServant ProxyNotConfiguredForService where
    showThentosError (ProxyNotConfiguredForService sid) = return (404, "proxy not configured for service " ++ show sid)

instance ThentosErrorShowServant NoSuchResetToken where
    showThentosError NoSuchResetToken = return (404, "no such password reset token")
-}


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
