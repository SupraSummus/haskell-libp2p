module Network.Libp2p.Transport.TCP (TCPListener, TCPDialer) where

import Network.Libp2p.Connection.Socket
import Data.Maybe (listToMaybe)
import Network.Socket (
        Socket,
        Family (AF_INET, AF_INET6),
        SocketType (Stream),
        SockAddr (SockAddrInet, SockAddrInet6),
        defaultProtocol, inet_addr,
        socket, close, bind, listen, connect
    )
import Control.Concurrent (forkIO)
import System.IO.Error (userError, ioError)
import Data.Function (fix)

data TCPListener = TCPListener Socket
data TCPDialer = TCPDialer

instance TransportListener TCPListener where

    createTransportListener multiaddr = do
        family, addr <- multiaddrToSockAddr multiaddr
        sock <- socket family Stream defaultProtocol
        bind sock addr
        listen sock 3
        return $ TCPListener sock

    closeTransportListener (TCPListener sock) = close sock

    accept (TCPListener sock) = liftM fst (accept sock)

instance TransportDialer where

    createTransportDialer = return TCPDialer

    closeTransportDialer TCPDialer = return ()

    dial TCPDialer multiaddr = do
        family, addr <- multiaddrToSockAddr multiaddr
        sock <- socket family Stream defaultProtocol
        connect sock addr
        return sock

multiaddrToSockAddr :: Multiaddr -> IO (Family, SockAddr)
multiaddrToSockAddr (a:b:c:d:e:f) = ioError $ userError "encapsulation is not supported"
multiaddrToSockAddr ("ip4":addrStr:"tcp":portStr:[]) = do
    addr <- inet_addr addrStr
    port <- readPort portStr
    return (AF_INET, SockAddrInet port addr)
multiaddrToSockAddr ("ip6":addrStr:"tcp":portStr:[]) = do
    addr <- inet_addr addrStr
    port <- readPort portStr
    return (AF_INET6, SockAddrInet6 port addr)
multiaddrToSockAddr _ = ioError $ userError "protocol is not supported"

readPort :: String -> IO PortNumber
readPort portStr = return $ maybe (error $ "invalid port " ++ portStr) (return . fst) (listToMaybe $ reads portStr)
