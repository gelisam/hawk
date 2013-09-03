import Test.Framework
--import Test.Framework.Providers.HUnit
--import Test.Framework.Providers.QuickCheck2
--import Test.HUnit
--import Test.QuickCheck

--import qualified Hawk.Test as HSPTest
import qualified System.Console.Hawk.Representable.Test as ReprTest
import qualified System.Console.Hawk.Config.Test as ConfigTest
import qualified System.Console.Hawk.Test as HawkTest

import Test.DocTest (doctest)
import Test.Hspec (hspec)

main :: IO ()
main = do
    hspec $ do
        ReprTest.spec
        ConfigTest.spec
    HawkTest.run
    doctest ["-isrc", "tests/System/Console/Hawk/Lock/Test.hs"]
