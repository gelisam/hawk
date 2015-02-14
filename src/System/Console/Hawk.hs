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

-- | Hawk as seen from the outside world: parsing command-line arguments,
--   evaluating user expressions.
module System.Console.Hawk
  ( processArgs
  ) where


import Control.Monad.Trans
import Control.Monad.Trans.Uncertain
import Data.HaskellExpr.Eval
import System.Console.Hawk.Args
import System.Console.Hawk.Args.Spec
import System.Console.Hawk.Help
import System.Console.Hawk.Interpreter
import System.Console.Hawk.Runtime.Base
import System.Console.Hawk.UserExpr.CanonicalExpr
import System.Console.Hawk.UserExpr.InputReadyExpr
import System.Console.Hawk.UserExpr.OriginalExpr
import System.Console.Hawk.Version


-- | Same as if the given arguments were passed to Hawk on the command-line.
processArgs :: [String] -> IO ()
processArgs args = do
    r <- runWarningsIO $ parseArgs args
    case r of
      Left err -> failHelp err
      Right spec -> processSpec spec

-- | A variant of `processArgs` which accepts a structured specification
--   instead of a sequence of strings.
processSpec :: HawkSpec -> IO ()
processSpec Help          = help
processSpec Version       = putStrLn versionString
processSpec (Eval  e   o) = processEvalSpec  (contextSpec e)   o (userExpr e)
processSpec (Apply e i o) = processApplySpec (contextSpec e) i o (userExpr e)
processSpec (Map   e i o) = processMapSpec   (contextSpec e) i o (userExpr e)

userExpr :: ExprSpec -> OriginalExpr
userExpr = originalExpr . untypedExpr


processEvalSpec :: ContextSpec -> OutputSpec -> OriginalExpr -> IO ()
processEvalSpec c o = runUncertainIO . processInputReadyExpr c noInput o . constExpr

processApplySpec :: ContextSpec -> InputSpec -> OutputSpec -> OriginalExpr -> IO ()
processApplySpec c i o = runUncertainIO . processInputReadyExpr c i o . applyExpr

processMapSpec :: ContextSpec -> InputSpec -> OutputSpec -> OriginalExpr -> IO ()
processMapSpec c i o = runUncertainIO . processInputReadyExpr c i o . mapExpr


processInputReadyExpr :: ContextSpec
                      -> InputSpec
                      -> OutputSpec
                      -> InputReadyExpr
                      -> UncertainT IO ()
processInputReadyExpr c i o e = case canonicalizeExpr i e of
    Just e' -> processCanonicalExpr c i o e'
    Nothing -> fail "conflicting flags"

processCanonicalExpr :: ContextSpec
                     -> InputSpec
                     -> OutputSpec
                     -> CanonicalExpr
                     -> UncertainT IO ()
processCanonicalExpr c i o e = do
    let contextDir = userContextDirectory c
    processRuntime <- runHawkInterpreter $ do
      applyContext contextDir
      interpretExpr (runCanonicalExpr e)
    lift $ runHawkIO $ processRuntime hawkRuntime
  where
    hawkRuntime :: HawkRuntime
    hawkRuntime = HawkRuntime i o
