{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

module Thentos.Backend.Core
where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar (MVar)
import Control.Exception (assert)
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

uncaughtThentosErrorServant :: (MonadIO m, ThentosError e) => e -> m (Int, String)
uncaughtThentosErrorServant e = do
    logThentosErrorServant CRITICAL "uncaught exception" e
    return (500, "internal error.")

logThentosErrorServant :: (MonadIO m, ThentosError e) => Priority -> String -> e -> m ()
logThentosErrorServant prio msg e = logger prio $ msg ++ " in servant: " ++ show e

-- | Render errors for servant.  (The servant error type will
-- hopefully change in the future.)
--

@@ -- no: this creates a closed/unextensible set of types that can be
   -- handled by servant backend.  see also '@@'-tagged remarks in
   -- "Thentos.Types".


renderThentosErrorServant :: (MonadIO m, ThentosError e) => e -> m (Int, String)
renderThentosErrorServant = f . toThentosError
  where
    q :: ThentosError e => SomeThentosError -> Maybe e
    q = fromThentosError

    r :: MonadIO m => Int -> String -> m (Int, String)
    r status msg = return (status, msg)

    f :: MonadIO m => SomeThentosError -> m (Int, String)
    f (q -> Just NoSuchUser)                           = r 404 "user not found"
    f (q -> Just NoSuchPendingUserConfirmation)        = r 404 "unconfirmed user not found"
    f (q -> Just NoSuchService)                        = r 404 "service not found"
    f (q -> Just NoSuchSession)                        = r 404 "session not found"
    f (q -> Just OperationNotPossibleInServiceSession) = r 404 "operation not possible in service session"
    f (q -> Just UserEmailAlreadyExists)               = r 403 "email already in use"
    f (q -> Just UserNameAlreadyExists)                = r 403 "user name already in use"
    f (q -> Just (PermissionDenied _ _ _))             = r 401 "unauthorized"
    f (q -> Just BadCredentials)                       = r 401 "bad credentials"
    f (q -> Just NoSuchResetToken)                     = r 404 "no such password reset token"
    f (q -> Just (MalformedConfirmationToken path))    = r 400 ("malformed confirmation token: " ++ show path)
    f e                                                = uncaughtThentosErrorServant e


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
    pushAction restState restAction = fmapLTM renderThentosErrorServant $ runReaderT restAction restState

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
fmapLTM :: (MonadIO m, Monad m, Functor m) => (a -> m a') -> EitherT a m b -> EitherT a' m b
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
