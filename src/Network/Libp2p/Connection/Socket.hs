module Network.Libp2p.Connection.Socket where

import Network.Libp2p.Connection.Class
import Network.Socket (
        Socket,
        ShutdownCmd (ShutdownSend),
        shutdown
    )
import Network.Socket.Bytestring (recv, sendAll)
import qualified ByteString as ByteString

instance StreamReadableConnection Socket where
    readConnection socket count = do
        if count == 0 then return ByteString.empty
        else recv socket count

instance WritableConnection Socket where
    writeConnection = sendAll
    closeConnection socket = shutdown ShutdownSend socket
