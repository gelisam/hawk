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

import Control.Applicative
  ((<$>))
import Data.ByteString.Lazy.Char8
  (ByteString)
import Language.Haskell.Interpreter
  (Extension)
import System.Directory
import System.FilePath
import System.IO
import Test.Hspec
import Test.HUnit
import GHC.IO.Handle

import Control.Monad.Trans.Uncertain
import System.Console.Hawk
import System.Console.Hawk.Context
import System.Console.Hawk.Context.Compatibility
import System.Console.Hawk.UserPrelude
import System.Console.Hawk.Options
import System.Console.Hawk.TestUtils
  (withTempDir')


--hawk :: Options                -- ^ Program options
--     -> (String,String)         -- ^ The prelude file and module name
--     -> [(String,Maybe String)] -- ^ The modules maybe qualified
--     -> [Extension]             -- ^ The extensions to enable
--     -> String                  -- ^ The user expression to evaluate
--     -> IO (Either InterpreterError (LB.ByteString -> LB.ByteString))

mode :: Mode
     -> Options
mode m = defaultOptions{ optMode = m }

run :: IO ()
run = withContextHSpec $ \itEval itApply itMap ->
        describe "Hawk" $ do
          itEval "" `into` ""
          itEval "1" `into` "1"
          itEval "1+1" `into` "2"
          itEval "[]" `into` ""
          itEval "[1]" `into` "1"
          itEval "[1,2]" `into` "1\n2"
          itEval "(1,2)" `into` "1\n2"
          itEval "[[1]]" `into` "1"
          itEval "[[1,2]]" `into` "1 2"
          itEval "[[1,2],[3,4]]" `into` "1 2\n3 4"
          
          itApply "id" `onInput` "foo" `into` "foo"
          itApply "L.transpose" `onInput` "1 2\n3 4" `into` "1 3\n2 4"
          itApply "L.map (!! 1)" `onInput` "1 2\n3 4" `into` "2\n4"

          itMap "(!! 1)" `onInput` "1 2\n3 4" `into` "2\n4"
  where onInput f x = f x
        into f x = f x

withContextHSpec :: ((String -> String -> Spec)
                    -> (String -> String -> String -> Spec)
                    -> (String -> String -> String -> Spec)
                    -> Spec)
                 -> IO ()
withContextHSpec body = withDefaultConfiguration $ \prelude modules extensions -> do
  let it' flags expr input expected =
        let descr = "evals " ++ show expr ++
                    " on input " ++ show input ++
                    " equals to " ++ show expected
        in it descr $ do
             tmpd <- getTemporaryDirectory
             (tmpf, tmph) <- openTempFile tmpd "hawk_input"
             str <- hPutStr tmph input
             hClose tmph
             out <- catchOutput $ do
               processArgs (flags ++ [expr, tmpf])
             removeFile tmpf
             assertEqual descr expected out
  let [itApply,itMap] = map it' [["-a"],["-m"]]
  let itEval expr expected = it' [] expr "" expected
  hspec $ body itEval itApply itMap


-- itEval <str> `withInput` <input> `equalsTo` <expected>

withDefaultConfiguration :: ((String,String) 
                             -> [(String,Maybe String)]
                             -> [Extension]
                             -> IO ())
                         -> IO ()
withDefaultConfiguration f = do
    context <- getContext "tests/preludes/default"

    f (configFromContext context)
      (modules context)
      (map read $ extensions context)


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
