
data Peer = Peer {address :: Multipath, id :: Multihash}

data Swarm = Swarm {
        handlers :: MVar (Map Multipath (c -> IO ()))
        transport :: MVar t
    }

dialPeer :: MessageConnection c => Swarm -> Peer -> Multipath -> IO c

handleSwarm :: s -> IO ()

handlers :: MessageConnection c => Swarm -> MVar (Map Multipath (c -> IO ()))

transport :: Transport t => s -> MVar t
