{-# LANGUAGE GADTs #-}
module Network.Libp2p.Connection.MessageToStream (messageToStream) where

import Control.Monad (liftM)
import Control.Concurrent.MVar
import Network.Libp2p.Connection.Class
import qualified Data.ByteString as ByteString

data MessageToStream where
    MessageToStream :: MessageConnection m => m -> MVar ByteString.ByteString -> MessageToStream

instance StreamReadableConnection MessageToStream where
    readStream (MessageToStream connection buffer) count = do
        buff <- takeMVar buffer
        readWithBuffers [buff] $ ByteString.length buff where
            readWithBuffers buffs len = do
                if len < count then do
                    msg <- readMessage connection
                    case msg of
                        Nothing -> concatBuffers buffs
                        Just d -> readWithBuffers (d:buffs) (len + ByteString.length d)
                else concatBuffers buffs
            concatBuffers buffs = do
                let buff = ByteString.concat $ reverse buffs
                putMVar buffer $ ByteString.drop count buff
                return $ ByteString.take count buff

instance WritableConnection MessageToStream where
    write (MessageToStream connection _) d = write connection d
    close (MessageToStream connection _) = close connection

instance StreamConnection MessageToStream

messageToStream :: MessageConnection m => m -> IO MessageToStream
messageToStream conn = (liftM $ MessageToStream conn) $ newMVar ByteString.empty
