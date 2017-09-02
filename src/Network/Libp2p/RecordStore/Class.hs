class RecordStore s where
    getRecord :: s -> Multihash -> IO [Multihash]
    putRecord :: s -> Multihash -> Multihash -> IO ()
