{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE DeriveDataTypeable                       #-}
{-# LANGUAGE ExistentialQuantification                #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE InstanceSigs                             #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE PackageImports                           #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TupleSections                            #-}
{-# LANGUAGE TypeFamilies                             #-}
{-# LANGUAGE TypeOperators                            #-}
{-# LANGUAGE TypeSynonymInstances                     #-}
{-# LANGUAGE UndecidableInstances                     #-}

module Thentos.Backend.Core
where

import Control.Applicative ((<$>))
import Control.Exception (assert)
import Control.Monad.Trans.Either  -- (EitherT(EitherT), runEitherT)
import Control.Monad.Trans.Reader (runReaderT)
import Data.CaseInsensitive (CI, mk, foldCase, foldedCase)
import Data.Char (isUpper)
import Data.Configifier ((>>.))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (cs)
import Data.String.Conversions (SBS, ST)
import Data.String (fromString)
import Data.Text.Encoding (decodeUtf8')
import Data.Typeable (Typeable)
import LIO.DCLabel (DCLabel, (%%))
import Network.HTTP.Types (Header)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (runSettings, setHost, setPort, defaultSettings)
import Network.Wai (ResponseReceived, Request, requestHeaders)
import Servant.API ((:<|>)((:<|>)))
import Servant.Server  -- (HasServer, Server, ServantErr, route)
import Servant.Server.Internal  -- ()
import Servant.Server.Internal.ServantErr  -- ()
import System.Log.Logger (Priority(DEBUG))

import qualified Data.ByteString.Char8 as SBS

import System.Log.Missing (logger)
import Thentos.Action.Core
import Thentos.Config
import Thentos.Types
import Thentos.Util


-- * action

enterAction :: DCLabel -> ActionState -> Action :~> EitherT ServantErr IO
enterAction clearance state = Nat $ EitherT . run
  where
    run :: Action a -> IO (Either ServantErr a)
    run = (>>= fmapLM actionErrorToServantErr) . runActionE clearance state


actionErrorToServantErr :: ActionError -> IO ServantErr
actionErrorToServantErr e = do
    -- (this will become more discriminating over time, but you get the idea: we can inspect the
    -- action error and then log stuff and return different stuff over the wire.)
    logger DEBUG $ show e
    return $ err500 { errBody = cs $ show e }



{-

-- | This is a work-around: The 'Server' type family terminates in
-- 'RestActionRaw' on all methods.  'PushActionC' instances transform
-- handlers implemented in a monad stack we want (providing acid
-- state, clearance info, random generator, ... in a reader) into the
-- handlers in 'RestActionRaw'.  (Also, translate 'DbError' to
-- 'RestError'.)
class PushActionC a where
    type PushActionSubRoute a
    pushAction :: ActionState -> PushActionSubRoute a -> a

instance (PushActionC b) => PushActionC (a -> b) where
    type PushActionSubRoute (a -> b) = a -> PushActionSubRoute b
    pushAction clearance f = pushAction clearance . f

instance (PushActionC a, PushActionC b) => PushActionC (a :<|> b) where
    type PushActionSubRoute (a :<|> b) = PushActionSubRoute a :<|> PushActionSubRoute b
    pushAction clearance (a :<|> b) = pushAction clearance a :<|> pushAction clearance b

instance PushActionC (RestActionRaw a) where
    type PushActionSubRoute (RestActionRaw a) = Action a
    pushAction restState restAction = assert False $ error "PushActionC (RestActionRaw a)"
                                        -- fmapLTM showThentosError $ runActionE clearance restState restAction
      where
        clearance = True %% False  -- FIXME

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
    type PushActionSubRoute (IO ResponseReceived) = Action ResponseReceived
    pushAction restState restAction = assert False $ error "PushActionC (IO ResponseReceived)"
                                -- (either crash id <$>) . runEitherT $ runReaderT restAction restState
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

-}



-- * header

data ThentosHeaderName =
    ThentosHeaderSession
  | ThentosHeaderService
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable)

lookupRequestHeader :: Request -> ThentosHeaderName -> Maybe ST
lookupRequestHeader req key =
          lookup (renderThentosHeaderName key) (requestHeaders req)
      >>= either (const Nothing) Just . decodeUtf8'

renderThentosHeaderName :: ThentosHeaderName -> CI SBS
renderThentosHeaderName x = case splitAt (SBS.length "ThentosHeader") (show x) of
    ("ThentosHeader", s) -> mk . SBS.pack $ "X-Thentos" ++ dashify s
    bad -> error $ "renderThentosHeaderName: prefix (left side) must be \"ThentosHeader\" in " ++ show bad
  where
    dashify ""    = ""
    dashify (h:t) = if isUpper h
        then '-' : h : dashify t
        else       h : dashify t

-- | Filter header list for all headers that start with "X-Thentos-", but have no parse in
-- 'ThentosHeaderName'.
badHeaders :: [Header] -> [Header]
badHeaders = filter g . filter f
  where
    f (k, _) = foldCase "X-Thentos-" `SBS.isPrefixOf` foldedCase k
    g (k, _) = not $ k `elem` map renderThentosHeaderName [minBound..]

-- | Make sure that all thentos headers are good ('badHeaders' yields empty list).
data ThentosAssertHeaders subserver = ThentosAssertHeaders subserver

instance (HasServer subserver) => HasServer (ThentosAssertHeaders subserver)
  where
    type ServerT (ThentosAssertHeaders subserver) m = ServerT subserver m

    route Proxy subserver request respond = case badHeaders $ requestHeaders request of
        []  -> route (Proxy :: Proxy subserver) subserver request respond
        bad -> respond . RR . Right . responseServantErr  -- FIXME: use left instead of this!
             $ err400 { errBody = cs $ "Unknown thentos header fields: " ++ show bad }


-- * warp

runWarpWithCfg :: HttpConfig -> Application -> IO ()
runWarpWithCfg cfg = runSettings settings
  where
    settings = setPort (cfg >>. (Proxy :: Proxy '["bind_port"]))
             . setHost (fromString . cs $ cfg >>. (Proxy :: Proxy '["bind_host"]))
             $ defaultSettings
