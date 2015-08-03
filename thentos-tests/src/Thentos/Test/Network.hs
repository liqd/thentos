module Thentos.Test.Network
where

import Network.Socket (bind, listen, socket, Family(AF_INET),
    SocketType(Stream), PortNumber, Socket, defaultProtocol, socketPort,
    aNY_PORT, SockAddr(SockAddrInet), inet_addr)


openTestSocket :: IO (PortNumber, Socket)
openTestSocket = do
    s <- socket AF_INET Stream defaultProtocol
    localhost <- inet_addr "127.0.0.1"
    bind s (SockAddrInet aNY_PORT localhost)
    listen s 1
    port <- socketPort s
    return (fromIntegral port, s)
