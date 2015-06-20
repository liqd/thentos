{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE DeriveDataTypeable                       #-}
{-# LANGUAGE DeriveGeneric                            #-}
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

-- | This is a variant of "Thentos.Backend.Api.Adhocracy3" that throws errors on the old
-- authentication end points and instead performs github authentication.  The intricacies of the
-- protocol look something like this:
--
-- >>> step req# description
-- >>>
-- >>> 01.  1.   BRO -> A3:    "can i give you access to my sso data on github so you know it's me?"
-- >>> 02.  1.   A3  -> BRO:   "ok, here is a request token."
-- >>> 03.  2.   BRO -> GIH:   "a3 gave me this request token."
-- >>> 04.  2.   GIH -> BRO:   "do you want to let a3 wants to access your name and t-shirt size?"
-- >>> 05.  3.   BRO -> GIH:   "sure: confirmed!"
-- >>> 06.  3.   GIH -> BRO:   "ok, please pass this access token on to a3."
-- >>> 07.  4.   BRO -> A3:    "here you go: take this to github and check out my t-shirt size."
-- >>> 08.  5.     A3  -> GIH: "here is an access token."
-- >>> 09.  5.     GIH -> A3:  "here is some accessed information: t-shirt size and name."
-- >>> 10.  4.   A3  -> BRO:   "here is your 'ThentosSessionToken'."
--
-- (A3 is represented by this module; the actual application first sees any traffic only after the
-- browser has received the response in step 8 and sends a new, authenticated request.)
module Thentos.Backend.Api.Adhocracy3Sso where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad.Except (throwError)
import Control.Monad (when, unless, mzero)
import Data.Aeson (Value(Object), ToJSON, FromJSON, (.:), (.:?), (.=), object, withObject)
import Data.Configifier ((>>.), Tagged(Tagged))
import Data.Functor.Infix ((<$$>))
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (ST, cs)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Network.Wai (Application)
import Safe (readMay)
import Servant.API ((:<|>)((:<|>)), (:>), Post, ReqBody, JSON)
import Servant.Server.Internal (Server)
import Servant.Server (serve, enter)
import Snap (urlEncode)  -- (not sure if this dependency belongs to backend?)
import System.Log (Priority(DEBUG, INFO))
import Text.Printf (printf)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as ST

import System.Log.Missing
import Thentos.Backend.Core
import Thentos.Config
import Thentos.Types
import Thentos.Util
import Thentos.Backend.Api.Proxy (ServiceProxy, serviceProxy)

import qualified Thentos.Action as A
import qualified Thentos.Action.Core as AC
import qualified Thentos.Backend.Api.Adhocracy3 as A3


-- * main

runBackend :: HttpConfig -> AC.ActionState DB -> IO ()
runBackend cfg asg = do
    logger INFO $ "running rest api (a3 style with SSO) on " ++ show (bindUrl cfg) ++ "."
    runWarpWithCfg cfg $ serveApi asg

serveApi :: AC.ActionState DB -> Application
serveApi = addResponseHeaders . serve (Proxy :: Proxy Api) . api


-- * api


type Api = {- ThenosA3Sso :<|> -} A3.ThentosApi :<|> ServiceProxy

-- | Like 'A3.thentosApi', but does connects error responses to the end points rather than handling
-- them as expected in "Thentos.Backend.Api.Adhocracy3".  This is to make sure the proxy handler
-- won't let any user management requests through that have been sent by confused clients.
thentosApi404 :: AC.ActionState DB -> Server A3.ThentosApi
thentosApi404 actionState = enter (enterAction actionState Nothing) $
       addUser
  :<|> activate
  :<|> login
  :<|> login

api :: AC.ActionState DB -> Server Api
api actionState =
       thentosApi404 actionState
  :<|> serviceProxy actionState


-- * handler

addUser :: A3.A3UserWithPass -> AC.Action DB (A3.A3Resource A3.A3UserNoPass)
addUser _ = error "404"  -- FIXME

activate :: A3.ActivationRequest -> AC.Action DB A3.RequestResult
activate _ = error "404"  -- FIXME

login :: A3.LoginRequest -> AC.Action DB A3.RequestResult
login _ = error "404"  -- FIXME
