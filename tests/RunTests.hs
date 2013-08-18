import Test.Framework
--import Test.Framework.Providers.HUnit
--import Test.Framework.Providers.QuickCheck2
--import Test.HUnit
--import Test.QuickCheck

--import qualified Hawk.Test as HSPTest
import qualified System.Console.Hawk.Representable.Test as ReprTest
import qualified System.Console.Hawk.Config.Test as ConfigTest

import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
    ReprTest.spec
    ConfigTest.spec
