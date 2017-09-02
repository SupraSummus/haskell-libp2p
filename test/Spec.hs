import Test.Hspec

import qualified Test.Connection.Local
import qualified Test.MultistreamSelector

main :: IO ()
main = hspec $ do
    Test.Connection.Local.test
    Test.MultistreamSelector.test
