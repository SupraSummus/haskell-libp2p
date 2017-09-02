module Test.Connection.Local where

import Test.Hspec
import Network.Libp2p.Connection.Class
import Network.Libp2p.Connection.Local
import qualified Data.ByteString as ByteString

test = describe "Local connection" $ do
    it "transmits data in both directions" $ do
        conns <- createConnectionPair
        write (fst conns) $ ByteString.pack [1, 2, 3, 4]
        write (snd conns) $ ByteString.pack [5, 6, 7]
        readMessage (fst conns) `shouldReturn` Just (ByteString.pack [5, 6, 7])
        readMessage (snd conns) `shouldReturn` Just (ByteString.pack [1, 2, 3, 4])

    it "transmits multiple, possibly empty messages" $ do
        conns <- createConnectionPair
        write (fst conns) $ ByteString.pack [1, 2, 3, 4]
        write (fst conns) $ ByteString.pack [5, 6, 7]
        write (fst conns) $ ByteString.empty
        write (fst conns) $ ByteString.empty
        write (fst conns) $ ByteString.pack [8]
        readMessage (snd conns) `shouldReturn` Just (ByteString.pack [1, 2, 3, 4])
        readMessage (snd conns) `shouldReturn` Just (ByteString.pack [5, 6, 7])
        readMessage (snd conns) `shouldReturn` Just (ByteString.empty)
        readMessage (snd conns) `shouldReturn` Just (ByteString.empty)
        readMessage (snd conns) `shouldReturn` Just (ByteString.pack [8])

    it "is closeable" $ do
        conns <- createConnectionPair
        close (snd conns)
        write (fst conns) $ ByteString.empty
        write (fst conns) $ ByteString.empty
        close (fst conns)
        readMessage (snd conns) `shouldReturn` Just (ByteString.empty)
        readMessage (snd conns) `shouldReturn` Just (ByteString.empty)
        readMessage (snd conns) `shouldReturn` Nothing
        readMessage (snd conns) `shouldReturn` Nothing

    it "can't be used for sending once it has been closed" $ do
        conns <- createConnectionPair
        close (fst conns)
        write (fst conns) ByteString.empty `shouldThrow` anyException

    it "can't be closed more than once" $ do
        conns <- createConnectionPair
        close (fst conns)
        close (fst conns) `shouldThrow` anyException
