{-# LANGUAGE LambdaCase #-}

module Network.HostAddr
where

import Data.Int (Int32)
import Network.Socket
          (SockAddr(SockAddrInet, SockAddrInet6, SockAddrUnix, SockAddrCan),
           HostAddress, HostAddress6, SocketType(Stream), AddrInfoFlag(AI_NUMERICHOST),
           addrAddress, getAddrInfo, addrFlags, addrSocketType, defaultHints)

data HostAddr
    = HostAddress  HostAddress
    | HostAddress6 HostAddress6
    | UnixAddress  String
    | CanAddress   Int32
    deriving (Eq, Read, Show)

hostAddr :: SockAddr -> HostAddr
hostAddr = \case
    SockAddrInet _ ip -> HostAddress ip
    SockAddrInet6 _ _ ip _ -> HostAddress6 ip
    SockAddrUnix fp -> UnixAddress fp
    SockAddrCan i -> CanAddress i

getHostAddr :: String -> IO HostAddr
getHostAddr name = hostAddr . addrAddress . single <$> getAddrInfo (Just hints) (Just name) Nothing
  where
    hints = defaultHints { addrFlags = [AI_NUMERICHOST], addrSocketType = Stream }

    single [x] = x
    single []  = error "Impossible error: numeric addresses should always resolve"
    single _   = error "Impossible error: too many results from getAddrInfo should have been ruled out by our `hints`"
