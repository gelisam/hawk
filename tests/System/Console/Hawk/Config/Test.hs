module System.Console.Hawk.Config.Test where

import System.Console.Hawk.Config (
    parseModules)
import Test.Hspec 

import System.Console.Hawk.TestUtils


spec :: Spec
spec = parseModulesSpec

parseModulesSpec :: Spec
parseModulesSpec = describe "parseModules" $ do
    it "returns empty when no modules are declared" $ do
        res <- withTempFile'' $ \file -> parseModules file []
        res `shouldBe` []
    it "returns the module with Nothing for unqualified imports" $ do
        res <- withTempFile'' $ \file -> do
            writeFile file "import Data.List"
            parseModules file []
        res `shouldBe` [("Data.List",Nothing)]
    it "returns the module with its qualification for qualified imports" $ do
        res <- withTempFile'' $ \file -> do
            writeFile file "import qualified Data.List as L"
            parseModules file []
        res `shouldBe` [("Data.List",Just "L")]
    it "returns the module both unqualified and with qualification for mixed" $ do
        res <- withTempFile'' $ \file -> do
            writeFile file "import Data.List as L"
            parseModules file []
        res `shouldBe` [("Data.List",Nothing),("Data.List",Just "L")]

    where withTempFile'' = withTempFile' "parseModulesSpec.hs"
