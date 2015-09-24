{-# LANGUAGE OverloadedStrings #-}

module Thentos.Test.Network
where

import Control.Concurrent.Async (Async, async, cancel, wait)
import Control.Exception (catch, AsyncException(ThreadKilled))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String.Conversions (LBS, SBS, cs)
import Network.HTTP.Types (Status, status200, status404)
import Network.Socket (bind, listen, socket, Family(AF_INET),
    SocketType(Stream), PortNumber, Socket, defaultProtocol, socketPort,
    aNY_PORT, SockAddr(SockAddrInet), inet_addr)
import Network.Wai (Application, rawPathInfo, responseLBS)

import qualified Data.Map as Map

openTestSocket :: IO (PortNumber, Socket)
openTestSocket = do
    s <- socket AF_INET Stream defaultProtocol
    localhost <- inet_addr "127.0.0.1"
    bind s (SockAddrInet aNY_PORT localhost)
    listen s 1
    port <- socketPort s
    return (fromIntegral port, s)


-- | Start a background processes.
startDaemon :: IO () -> IO (Async ())
startDaemon = async

-- | Stop a background processes.
stopDaemon :: Async () -> IO ()
stopDaemon a = do
    cancel a
    catch (wait a) (\ThreadKilled -> return ())

-- | Simple server that replies to all requests with the same response.
-- Takes an optional status code (default: 200 OK), an optional content type (default:
-- application/json), and a response body.
staticReplyServer :: Maybe Status -> Maybe SBS -> LBS -> Application
staticReplyServer mStatus mContentType respBody _req respond =
    respond $ responseLBS status [("Content-Type", contentType)] respBody
  where
    status      = fromMaybe status200 mStatus
    contentType = fromMaybe "application/json" mContentType

-- | Somewhat more refined reply server that looks up the route in a map and replies with the status
-- and response defined for that route. Routes must match *exactly,* not just by prefix. All
-- replies must have the same content type (first argument, default: application/json).
-- Returns 404 and a simple plain-text body if no matching route is found.
routingReplyServer :: Maybe SBS -> Map.Map SBS (Status, LBS) -> Application
routingReplyServer mContentType replyMap req respond =
    case Map.lookup route replyMap of
        Just (status, respBody) -> respond $ responseLBS status
            [("Content-Type", contentType)] respBody
        Nothing                 -> respond . responseLBS status404
            [("Content-Type", "text/plain; charset=UTF-8")] $ "Route not found: " <> cs route
  where
    route       = rawPathInfo req
    contentType = fromMaybe "application/json" mContentType
