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

import System.Environment (unsetEnv)
import Test.DocTest (doctest)
import Test.Hspec (hspec)

import qualified Build_doctests as CabalDoctest
import qualified System.Console.Hawk.Representable.Test as ReprTest
import qualified System.Console.Hawk.Test as HawkTest


main :: IO ()
main = do
    unsetEnv "GHC_ENVIRONMENT" -- as explained in the cabal-doctest documentation
    doctest $ CabalDoctest.flags
           ++ CabalDoctest.pkgs
           ++ CabalDoctest.module_sources
    doctest $ CabalDoctest.flags_exe_hawk
           ++ CabalDoctest.pkgs_exe_hawk
           ++ CabalDoctest.module_sources_exe_hawk

    hspec $ do
        ReprTest.reprSpec'
        ReprTest.reprSpec
    HawkTest.run
