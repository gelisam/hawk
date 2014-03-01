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

{-# LANGUAGE OverloadedStrings #-}
module System.Console.Hawk.Test where

import System.Directory
import System.IO
import Test.Hspec
import Test.HUnit
import GHC.IO.Handle

import System.Console.Hawk


run :: IO ()
run = withContextHSpec $ \itEval itApply itMap ->
        describe "Hawk" $ do
          itEval "" `into` "\n"
          itEval "1" `into` "1\n"
          itEval "1+1" `into` "2\n"
          itEval "[]" `into` ""
          itEval "[1]" `into` "1\n"
          itEval "[1,2]" `into` "1\n2\n"
          itEval "(1,2)" `into` "1\n2\n"
          itEval "[[1]]" `into` "1\n"
          itEval "[[1,2]]" `into` "1 2\n"
          itEval "[[1,2],[3,4]]" `into` "1 2\n3 4\n"
          
          itApply "id" `onInput` "foo" `into` "foo\n"
          itApply "L.transpose" `onInput` "1 2\n3 4" `into` "1 3\n2 4\n"
          itApply "L.map (!! 1)" `onInput` "1 2\n3 4" `into` "2\n4\n"

          itMap "(!! 1)" `onInput` "1 2\n3 4" `into` "2\n4\n"
  where onInput f x = f x
        into f x = f x

withContextHSpec :: ((String -> String -> Spec)
                    -> (String -> String -> String -> Spec)
                    -> (String -> String -> String -> Spec)
                    -> Spec)
                 -> IO ()
withContextHSpec body = do
  let it' flags expr input expected =
        let descr = "evals " ++ show expr ++
                    " on input " ++ show input ++
                    " equals to " ++ show expected
        in it descr $ do
             tmpd <- getTemporaryDirectory
             (tmpf, tmph) <- openTempFile tmpd "hawk_input"
             hPutStr tmph input
             hClose tmph
             out <- catchOutput $ do
               processArgs $ concat [ ["-c", "tests/preludes/default"]
                                    , flags
                                    , [expr, tmpf]
                                    ]
             removeFile tmpf
             assertEqual descr expected out
  let [itApply,itMap] = map it' [["-a"],["-m"]]
  let itEval expr expected = it' [] expr "" expected
  hspec $ body itEval itApply itMap


-- from http://stackoverflow.com/a/9664017/3286185
catchOutput :: IO () -> IO String
catchOutput f = do
    tmpd <- getTemporaryDirectory
    (tmpf, tmph) <- openTempFile tmpd "haskell_stdout"
    stdout_dup <- hDuplicate stdout
    hDuplicateTo tmph stdout
    hClose tmph
    f
    hDuplicateTo stdout_dup stdout
    str <- readFile tmpf
    removeFile tmpf
    return str
