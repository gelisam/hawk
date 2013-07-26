import Data.Monoid (mempty)

import Test.Framework
--import Test.Framework.Providers.HUnit
--import Test.Framework.Providers.QuickCheck2
--import Test.HUnit
--import Test.QuickCheck

--import qualified HSProcess.Test as HSPTest
import qualified HSProcess.Representable.Test as ReprTest

main :: IO ()
main = defaultMainWithOpts
        [ testGroup "Representable" ReprTest.tests
        ] mempty
--main = defaultMainWithOpts
--       [ testCase "rev" testRev
--       , testProperty "listRevRevId" propListRevRevId
--       ] mempty

-- testRev :: Assertion
-- testRev = reverse [1, 2, 3] @?= [3, 2, 1]
