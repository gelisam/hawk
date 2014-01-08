--   Copyright 2013 Mario Pastorelli (pastorelli.mario@gmail.com) Samuel Gélineau (gelisam@gmail.com)
--
--   Licensed under the Apache License, Version 2.0 (the "License");
--   you may not use this file except in compliance with the License.
--   You may obtain a copy of the License at
--
--       http://www.apache.org/licenses/LICENSE-2.0
--
--   Unless required by applicable law or agreed to in writing, software
--   distributed under the License is distributed on an "AS IS" BASIS,
--   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--   See the License for the specific language governing permissions and
--   limitations under the License.

module System.Console.Hawk.Config.Test where

import Language.Haskell.Exts.Parser (
    ParseResult (..)
    )

import System.Console.Hawk.Config.Parse (
    parseModules)
import Test.Hspec 


instance Show a => Eq (ParseResult a) where
  p == q = show p == show q

spec :: Spec
spec = parseModulesSpec

parseModulesSpec :: Spec
parseModulesSpec = describe "parseModules" $ do
    it "returns empty when no modules are declared" $ do
        parseModules [] ""
          `shouldBe` ParseOk []
    it "returns the module with Nothing for unqualified imports" $ do
        parseModules [] "import Data.List"
          `shouldBe` ParseOk [("Data.List",Nothing)]
    it "returns the module with its qualification for qualified imports" $ do
        parseModules [] "import qualified Data.List as L"
          `shouldBe` ParseOk [("Data.List",Just "L")]
    it "returns the module both unqualified and with qualification for mixed" $ do
        parseModules [] "import Data.List as L"
          `shouldBe` ParseOk [("Data.List",Nothing),("Data.List",Just "L")]
