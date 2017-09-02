module Test.MultistreamSelector (test) where

import Test.Hspec

import Network.Libp2p.MultistreamSelector
import Network.Libp2p.Connection.Class
import Network.Libp2p.Connection.Local
import Network.Libp2p.Connection.MessageToStream
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString as ByteString
import Data.Bytes.Varint
import Codec.Multipath (Multipath (Multipath))

test = describe "MultistreamSelector" $ do

    describe "serveMultistreamSelect" $ do
        it "serves protocol negotiation" $ do
            testConnections <- createConnectionPair
            testConnectionA <- messageToStream $ fst testConnections
            testConnectionB <- messageToStream $ snd testConnections

            mapM_ (write testConnectionB) [
                    varintToByteString 19, UTF8.fromString "/multistream/1.0.0\n",  -- handshake
                    varintToByteString 8, UTF8.fromString "/test/c\n",
                    varintToByteString 3, UTF8.fromString "ls\n",
                    varintToByteString 8, UTF8.fromString "/test/a\n",
                    ByteString.pack [0x42]                                          -- protocol specific data
                ]
            close testConnectionB

            serveMultistreamSelect testConnectionA [
                    Multipath ["test", "a"],
                    Multipath ["test", "b"]
                ] `shouldReturn` Multipath ["test", "a"]
            readStream testConnectionA 1 `shouldReturn` ByteString.pack [0x42]
            readStream testConnectionA 1 `shouldReturn` ByteString.empty
            write testConnectionA $ ByteString.pack [0x56]
            close testConnectionA

            readStream testConnectionB 1000 `shouldReturn` ByteString.concat [
                    varintToByteString 19, UTF8.fromString "/multistream/1.0.0\n",                           -- handshake
                    varintToByteString 3, UTF8.fromString "na\n",                                            -- /test/c not available
                    varintToByteString 3, varintToByteString 18, varintToByteString 2, UTF8.fromString "\n", -- reply to ls
                    varintToByteString 8, UTF8.fromString "/test/a\n",
                    varintToByteString 8, UTF8.fromString "/test/b\n",
                    varintToByteString 8, UTF8.fromString "/test/a\n",                                       -- ok, lets talk /test/a
                    ByteString.pack [0x56]                                                                   -- protocol specific data
                ]
