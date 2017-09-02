class PeerRouter r where
    findPeers :: r -> Multihash -> IO [PeerInfo]
