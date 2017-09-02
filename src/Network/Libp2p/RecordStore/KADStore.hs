data KADStore = KADStore KADRouter Swarm

instance RecordStore KADStore where
    getRecord (KADStore kadRouter swarm) key = liftM concat . mapM (askPeerForRecords swarm key) (findPeers kadRouter key)

askPeerForRecords swarm key peer = do
    conn <- dial swarm peer $ Multipath ["ipfs", "kad-record-store", "1.0.0", "get"]
    write conn $ Multihash.toByteString multihash
    close conn
    reply <- readAll conn
    


swarmSingleDial swarm peer protocol bs = do
    conn <- dial swarm peer protocol
    write conn bs
    close conn
    readAll conn
