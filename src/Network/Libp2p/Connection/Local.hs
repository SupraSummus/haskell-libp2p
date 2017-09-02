module Network.Libp2p.Connection.Local (createConnectionPair) where

import Network.Libp2p.Connection.Class
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import qualified Data.ByteString as ByteString

data LocalConnection = LocalConnection {
        closed :: MVar Bool,
        inputChan :: Chan (Maybe ByteString.ByteString),
        outputChan :: Chan (Maybe ByteString.ByteString)
    }

instance MessageReadableConnection LocalConnection where
    readMessage conn = do
        msg <- readChan $ inputChan conn
        case msg of
            Nothing -> do
                writeChan (inputChan conn) Nothing
                return Nothing
            Just d -> return $ Just d

instance WritableConnection LocalConnection where
    write conn d = do
        c <- takeMVar $ closed conn
        if c then do
            putMVar (closed conn) True
            ioError $ userError "connection is closed for writing"
        else do
            writeChan (outputChan conn) $ Just d
            putMVar (closed conn) False

    close conn = do
        c <- takeMVar $ closed conn
        if c then do
            putMVar (closed conn) True
            ioError $ userError "connection already closed"
        else do
            writeChan (outputChan conn) Nothing
            putMVar (closed conn) True

instance MessageConnection LocalConnection

createConnectionPair :: IO (LocalConnection, LocalConnection)
createConnectionPair = do
    chanA <- newChan
    chanB <- newChan
    closedA <- newMVar False
    closedB <- newMVar False
    return (
            LocalConnection closedA chanB chanA,
            LocalConnection closedB chanA chanB
        )
    
