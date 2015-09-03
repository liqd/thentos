{-# LANGUAGE OverloadedStrings #-}

module Thentos.Test.Network
where

import Control.Concurrent.Async (Async, async, cancel, wait, link)
import Control.Exception (catch, AsyncException(ThreadKilled))
import Data.Maybe (fromMaybe)
import Data.String.Conversions (LBS, SBS)
import Network.HTTP.Types (Status, status200)
import Network.Socket (bind, listen, socket, Family(AF_INET),
    SocketType(Stream), PortNumber, Socket, defaultProtocol, socketPort,
    aNY_PORT, SockAddr(SockAddrInet), inet_addr)
import Network.Wai (Application, responseLBS)

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
startDaemon x = do
    a <- async x
    link a
    return a

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
