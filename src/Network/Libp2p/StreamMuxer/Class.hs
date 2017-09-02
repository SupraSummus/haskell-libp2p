module Network.Libp2p.StreamMuxer.Class where

import Network.Libp2p.Connection.Class

class StreamMuxer m where
    createStreamMuxer :: StreamConnection c => c -> IO m
    closeStreamMuxer :: m -> IO ()
    dialStream :: MessageConnection c => m -> IO c
    acceptStream :: MessageConnection c =>  m -> IO c
