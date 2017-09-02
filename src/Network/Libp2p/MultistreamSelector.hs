module Network.Libp2p.MultistreamSelector where

import Network.Libp2p.Connection.Class
import qualified Data.ByteString as ByteString
import qualified Codec.Multipath as Multipath
import Control.Monad (when, liftM)
import Control.Exception (ioError, catch)
import Data.Bytes.Varint

multistreamVersion = Multipath.Multipath ["multistream", "1.0.0"]
lsMessage = ByteString.pack [108, 115] -- "ls"
naMessage = ByteString.pack [110, 97] -- "na"
newlineChar = 10 -- newline

recvHeader :: StreamConnection c => c -> IO ()
recvHeader = expectProtocol multistreamVersion

expectProtocol :: StreamConnection c => Multipath.Multipath -> c -> IO ()
expectProtocol expectedProtocol conn = do
    msg <- recvMessage conn
    protocol <- either (\err -> closeConnectionWithError conn $ "protocol name parse failed: " ++ err) return (Multipath.fromByteString msg)
    when (protocol /= expectedProtocol) $ do
        closeConnectionWithError conn (
            "expected protocol " ++ show expectedProtocol ++
            " but got " ++ show protocol)

recvMessage :: StreamConnection c => c -> IO ByteString.ByteString
recvMessage conn = do
    len <- supplyVarint (liftM ByteString.head $ readConnectionOrFail "expected varint byte" conn 1)
    when (len < 1) $ closeConnectionWithError conn "expected payload length to be at least 1"
    payload <- readConnectionOrFail "expected payload bytes" conn (len - 1)
    newline <- readConnectionOrFail "expected newline byte" conn 1
    when (ByteString.head newline /= newlineChar) $ closeConnectionWithError conn "expected payload to end with newline"
    return payload

readConnectionOrFail :: StreamConnection c => String -> c -> Int -> IO ByteString.ByteString
readConnectionOrFail errmsg conn n = do
    d <- readStream conn n
    if ByteString.length d /= n then closeConnectionWithError conn (
            errmsg ++ ": expected " ++ show n ++
            " but got " ++ show (ByteString.length d) ++ " bytes"
        )
    else return d

sendHeader :: WritableConnection c => c -> IO ()
sendHeader = sendProtocol multistreamVersion

sendLS :: WritableConnection c => c -> IO ()
sendLS conn = sendMessage conn lsMessage

sendNA :: WritableConnection c => c -> IO ()
sendNA conn = sendMessage conn naMessage

sendProtocol :: WritableConnection c => Multipath.Multipath -> c -> IO ()
sendProtocol protocol conn = sendMessage conn $ Multipath.toByteString protocol

sendProtocols :: WritableConnection c => [Multipath.Multipath] -> c -> IO ()
sendProtocols protocols conn = do
    let message = ByteString.concat $ map (makeMessage . Multipath.toByteString) protocols
    sendMessage conn $ ByteString.pack (
            (varintEncode $ ByteString.length message) ++
            (varintEncode $ length protocols)
        )
    write conn message

sendMessage :: WritableConnection c => c -> ByteString.ByteString -> IO ()
sendMessage conn msg = write conn $ makeMessage msg

makeMessage :: ByteString.ByteString -> ByteString.ByteString
makeMessage msg = ByteString.concat [
        ByteString.pack $ varintEncode (ByteString.length msg + 1),
        msg,
        ByteString.pack [newlineChar]
    ]

closeConnectionWithError :: WritableConnection c => c -> String -> IO a
closeConnectionWithError conn err = do
    close conn
    ioError $ userError err

serveMultistreamSelect :: StreamConnection c => c -> [Multipath.Multipath] -> IO Multipath.Multipath
serveMultistreamSelect conn protocols = do
    recvHeader conn
    sendHeader conn
    handleMessage where
        handleMessage = do
            msg <- recvMessage conn
            if msg == lsMessage then do
                sendProtocols protocols conn
                handleMessage
            else case Multipath.fromByteString msg of
                Left err -> closeConnectionWithError conn $ "protocol name parse failed: " ++ err
                Right protocol -> if elem protocol protocols then do
                        sendProtocol protocol conn
                        return protocol
                    else do
                        sendNA conn
                        handleMessage

dialMultistreamSelect :: StreamConnection c => c -> [Multipath.Multipath] -> IO (Maybe Multipath.Multipath)
dialMultistreamSelect conn allProtocols = do
    sendHeader conn
    recvHeader conn
    tryProtocols allProtocols where
        tryProtocols [] = do
            close conn
            return Nothing
        tryProtocols (protocol:protocols) = do
            sendProtocol protocol conn
            msg <- recvMessage conn
            if msg == naMessage then tryProtocols protocols
            else if msg == Multipath.toByteString protocol then return $ Just protocol
            else closeConnectionWithError conn "unexpected response to proposed protocol"
