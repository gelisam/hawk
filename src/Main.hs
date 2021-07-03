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
{-# LANGUAGE CPP #-}

module Main where

#if !MIN_VERSION_simple_cmd_args(0,1,3)
import Control.Applicative ((<|>))
#endif
import SimpleCmdArgs
import System.FilePath

import System.Console.Hawk
import System.Console.Hawk.Args.Spec
import System.Console.Hawk.Context.Dir
import System.Console.Hawk.Version

main :: IO ()
main = do
  defaultContextDir <- findContextFromCurrDirOrDefault
  simpleCmdArgs (Just version) "A Haskell awk/sed like tool"
    "shell text processing with Haskell" $
    processSpec
      <$> ( ExprSpec
        <$> (ContextSpec <$> cfgdirOpt defaultContextDir)
        <*> strArg "EXPR"
          )
      <*> modeOpt
  where
    modeOpt :: Parser HawkMode
    modeOpt =
      flagWith' LineMode 'l' "line" "Apply function to each line" <|>
      flagWith' WordsMode 'w' "words" "Apply function to list of words per line" <|>
      flagWith' WholeMode 'a' "all" "Apply function once to the whole input" <|>
      flagWith' TypeMode 't' "typecheck" "Print out the type of the given function" <|>
      flagWith' EvalMode 'e' "eval" "Evaluate a Haskell expression" <|>
      flagWith' RunMode 'r' "run" "Run Haskell IO"

    cfgdirOpt :: FilePath -> Parser FilePath
    cfgdirOpt dir
      = normalise <$>
        strOptionalWith 'c' "config-dir" "DIR" ("Override the config dir [default:" ++ dir ++ "]") dir
