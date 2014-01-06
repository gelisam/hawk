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

import qualified System.Console.Hawk.Representable.Test as ReprTest
import qualified System.Console.Hawk.Config.Test as ConfigTest
import qualified System.Console.Hawk.Test as HawkTest

import Test.DocTest (doctest)
import Test.Hspec (hspec)

main :: IO ()
main = do
    doctest ["-isrc", "src/System/Console/Hawk/Config/Cache.hs"]
    doctest ["-isrc", "tests/System/Console/Hawk/Lock/Test.hs"]
    hspec $ do
        ReprTest.reprSpec'
        ReprTest.reprSpec
        ConfigTest.spec
    HawkTest.run
