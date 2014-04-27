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

import Data.List
import qualified System.Console.Hawk.Representable.Test as ReprTest
import qualified System.Console.Hawk.Test as HawkTest
import System.Environment
import Text.Printf

import Test.DocTest (doctest)
import Test.Hspec (hspec)


substSuffix :: Eq a => [a] -> [a] -> [a] -> [a]
substSuffix oldSuffix newSuffix xs | oldSuffix `isSuffixOf` xs = prefix ++ newSuffix
  where
    prefix = take (length xs - length oldSuffix) xs
substSuffix _ _ xs = xs

-- make sure doctest can the source of Hawk and the generated Paths_haskell_awk.hs
doctest' :: String -> IO ()
doctest' file = do
    exePath <- getExecutablePath
    
    let srcPath = "src"
    let autogenPath = substSuffix "reference/reference" "autogen" exePath
    
    let includeSrc      = printf "-i%s" srcPath
    let includeAutogen  = printf "-i%s" autogenPath
    
    doctest [includeSrc, includeAutogen, file]

main :: IO ()
main = do
    doctest' "tests/System/Console/Hawk/Lock/Test.hs"
    doctest' "src/Data/Cache.hs"
    doctest' "src/Data/HaskellSource.hs"
    doctest' "src/Data/HaskellModule.hs"
    doctest' "src/Data/HaskellModule/Parse.hs"
    doctest' "src/System/Console/Hawk.hs"
    doctest' "tests/System/Console/Hawk/PreludeTests.hs"
    doctest' "tests/Data/HaskellModule/Parse/Test.hs"
    doctest' "src/System/Console/Hawk/Args/Option.hs"
    doctest' "src/System/Console/Hawk/Args/Parse.hs"
    doctest' "src/System/Console/Hawk/UserPrelude.hs"
    doctest' "src/System/Console/Hawk/UserPrelude/Extend.hs"
    doctest' "src/Control/Monad/Trans/Uncertain.hs"
    doctest' "src/Control/Monad/Trans/OptionParser.hs"
    hspec $ do
        ReprTest.reprSpec'
        ReprTest.reprSpec
    HawkTest.run
