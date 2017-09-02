module Network.Libp2p.Connection.Class where

import Network.Libp2p.Data (PeerId)
import Data.ByteString (ByteString)

class WritableConnection c where
    write :: c -> ByteString -> IO ()
    close :: c -> IO ()                             -- close for writing

class MessageReadableConnection c where
    readMessage :: c -> IO (Maybe ByteString)

class StreamReadableConnection c where
    readStream :: c -> Int -> IO ByteString         -- Must return requested number of bytes.
                                                    -- Smaller number of returned bytes indicates that connection has been closed.

class (WritableConnection c, StreamReadableConnection c) => StreamConnection c

class (WritableConnection c, MessageReadableConnection c) => MessageConnection c
