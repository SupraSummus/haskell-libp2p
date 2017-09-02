module Network.Libp2p.Class where

import Network.Libp2p.Connection.Class
import Network.Libp2p.Data (PeerId)

class Libp2p l where
    dialPeer :: StreamConnection c => l -> PeerId -> IO c
    accept :: StreamConnection c => l -> IO c
