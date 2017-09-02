

instance StreamConnection c => TransportDialer [Multiaddr -> IO (Maybe c)] where
    dial [] addr = return Nothing
    dial (dialer:dialres) addr = do
        maybeConn <- dialer addr
        case maybeConn of
            Nothing -> dial dialers addr
            Just conn -> return conn
