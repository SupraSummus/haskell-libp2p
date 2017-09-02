module Network.Libp2p.Transport.Class where

import Network.Libp2p.Connection.Class

class TransportListener tl where
    createTransportListener :: Multiaddr -> IO (Maybe tl)
    closeTransportListener :: tl -> IO ()
    accept :: StreamConnection c => tl -> IO c

class TransportDialer td where
    dial :: StreamConnection c => td -> Multiaddr -> IO (Maybe c)

