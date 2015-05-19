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
import Control.Monad.Trans.Either (EitherT(EitherT))
import Data.CaseInsensitive (CI, mk, foldCase, foldedCase)
import Data.Char (isUpper)
import Data.Configifier ((>>.))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (cs)
import Data.String.Conversions (SBS, ST)
import Data.String (fromString)
import Data.Text.Encoding (decodeUtf8')
import Data.Typeable (Typeable)
import Network.HTTP.Types (Header)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (runSettings, setHost, setPort, defaultSettings)
import Network.Wai (Request, requestHeaders)
import Servant.API ((:>))
import Servant.Server (HasServer, ServerT, ServantErr, route, (:~>)(Nat))
import Servant.Server.Internal (RouteResult(RR))
import Servant.Server.Internal.ServantErr (err400, err500, errBody, responseServantErr)
import System.Log.Logger (Priority(DEBUG))

import qualified Data.ByteString.Char8 as SBS
import qualified Network.HTTP.Types.Header as HttpTypes

import System.Log.Missing (logger)
import Thentos.Action.Core
import Thentos.Config
import Thentos.Types
import Thentos.Util


-- * action

enterAction :: ActionState DB -> Maybe ThentosSessionToken -> Action DB :~> EitherT ServantErr IO
enterAction state mTok = Nat $ EitherT . run
  where
    run :: Action DB a -> IO (Either ServantErr a)
    run = (>>= fmapLM actionErrorToServantErr) . runActionE state . updatePrivs mTok

    updatePrivs :: Maybe ThentosSessionToken -> Action DB a -> Action DB a
    updatePrivs (Just tok) action = (privsByThentosSession'P tok >>= setClearance'P) >> action
    updatePrivs Nothing    action = action


actionErrorToServantErr :: ActionError -> IO ServantErr
actionErrorToServantErr e = do
    -- (this will become more discriminating over time, but you get the idea: we can inspect the
    -- action error and then log stuff and return different stuff over the wire.)
    logger DEBUG $ show e
    return $ err500 { errBody = cs $ show e }


-- * header

data ThentosHeaderName =
    ThentosHeaderSession
  | ThentosHeaderService
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable)

lookupThentosHeader :: Request -> ThentosHeaderName -> Maybe ST
lookupThentosHeader req key =
          lookup (renderThentosHeaderName key) (requestHeaders req)
      >>= either (const Nothing) Just . decodeUtf8'

lookupThentosHeaderSession :: Request -> Maybe ThentosSessionToken
lookupThentosHeaderSession req = ThentosSessionToken <$> lookupThentosHeader req ThentosHeaderSession

lookupThentosHeaderService :: Request -> Maybe ServiceId
lookupThentosHeaderService req = ServiceId <$> lookupThentosHeader req ThentosHeaderService

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

-- | Remove all headers that match @X-Thentos-.*@.  This is useful if the request is to be used as a
-- basis for e.g. constructing another request to a proxy target.
clearThentosHeaders :: HttpTypes.RequestHeaders -> HttpTypes.RequestHeaders
clearThentosHeaders = filter $ (foldedCase "X-Thentos-" `SBS.isPrefixOf`) . foldedCase . fst

-- | Make sure that all thentos headers are good ('badHeaders' yields empty list).
data ThentosAssertHeaders = ThentosAssertHeaders

instance (HasServer subserver) => HasServer (ThentosAssertHeaders :> subserver)
  where
    type ServerT (ThentosAssertHeaders :> subserver) m = ServerT subserver m

    route Proxy subserver request respond = case badHeaders $ requestHeaders request of
        []  -> route (Proxy :: Proxy subserver) subserver request respond
        bad -> respond . RR . Right . responseServantErr  -- FIXME: use 'left' instead of all this?  yields a type error, though.
             $ err400 { errBody = cs $ "Unknown thentos header fields: " ++ show bad }


-- * warp

runWarpWithCfg :: HttpConfig -> Application -> IO ()
runWarpWithCfg cfg = runSettings settings
  where
    settings = setPort (cfg >>. (Proxy :: Proxy '["bind_port"]))
             . setHost (fromString . cs $ cfg >>. (Proxy :: Proxy '["bind_host"]))
             $ defaultSettings
