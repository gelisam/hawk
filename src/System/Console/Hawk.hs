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
{-# LANGUAGE CPP #-}

module System.Console.Hawk
  ( processSpec,
    HawkMode(..)
  ) where

import Prelude hiding (fail)

#if MIN_VERSION_base(4,12,0)
import Control.Monad.Fail (fail)
#else
import Prelude (fail)
#endif
import Control.Monad.Trans
import Data.List
import Language.Haskell.Interpreter

import Control.Monad.Trans.Uncertain
import Data.HaskellExpr.Eval
import System.Console.Hawk.Args
import System.Console.Hawk.Args.Spec
import System.Console.Hawk.Interpreter
import System.Console.Hawk.Runtime.Base
import System.Console.Hawk.UserExpr.CanonicalExpr
import System.Console.Hawk.UserExpr.InputReadyExpr
import System.Console.Hawk.UserExpr.OriginalExpr


data HawkMode = DefaultMode | LineMode | WordsMode | WholeMode | TypeMode | EvalMode | RunMode | ShellMode
  deriving Eq

-- | A variant of `processArgs` which accepts a structured specification
--   instead of a sequence of strings.
processSpec
  :: ExprSpec -> HawkMode -> IO ()
processSpec e EvalMode
  = myRunUncertainIO e
  $ processEvalSpec (contextSpec e) defaultOutputSpec (userExpr e)
processSpec e WholeMode
  = myRunUncertainIO e
  $ processApplySpec (contextSpec e) defaultInputSpec defaultOutputSpec (userExpr e)
processSpec e LineMode
  = myRunUncertainIO e
  $ processMapSpec (contextSpec e) defaultInputSpec defaultOutputSpec (userExpr e)

-- | A version of `runUncertainIO` which detects poor error messages and improves them.
myRunUncertainIO :: ExprSpec -> UncertainT IO () -> IO ()
myRunUncertainIO e = runUncertainIO . clarifyErrors e
  where
    clarifyErrors :: ExprSpec -> UncertainT IO () -> UncertainT IO ()
    clarifyErrors exprSpec body = do
        r <- lift $ runUncertainT body
        case r of
          (Left errorMsg, _) | fromWrapperCode errorMsg -> do
            -- try again without the wrapper code
            let contextDir = userContextDirectory (contextSpec exprSpec)
            runHawkInterpreter contextDir $ do
              applyContext contextDir
              interpret annotatedExpr
                        (as :: ())  -- not the right type, but it should
                                    -- error-out before type-checking anyway.
            
            -- Unfortunately, if we messed up and the error really was in the wrapper
            -- code, the above will tell the user that the problem is that their user
            -- expression did not have type unit. In the unlikely case that it did
            -- have type unit, tell them that we messed up.
            warn "this should not happen."
            warn "please report this to https://github.com/gelisam/hawk/issues/new"
          _ -> return ()
        
        -- we didn't succeed at replacing the error (of we would have aborted by now).
        -- rethrow the original errors and warnings.
        uncertainT r
      where
        annotatedExpr = "{-# LINE 1 \"user expression\" #-}\n" ++ untypedExpr exprSpec
    
    fromWrapperCode :: String -> Bool
    fromWrapperCode msg = "\twrapper code:" `isPrefixOf` firstLine
      where
        firstLine = (lines msg ++ ["",""]) !! 1

userExpr :: ExprSpec -> OriginalExpr
userExpr = originalExpr . untypedExpr


processEvalSpec :: ContextSpec -> OutputSpec -> OriginalExpr -> UncertainT IO ()
processEvalSpec c o = processInputReadyExpr c noInput o . constExpr

processApplySpec :: ContextSpec -> InputSpec -> OutputSpec -> OriginalExpr -> UncertainT IO ()
processApplySpec c i o = processInputReadyExpr c i o . applyExpr

processMapSpec :: ContextSpec -> InputSpec -> OutputSpec -> OriginalExpr -> UncertainT IO ()
processMapSpec c i o = processInputReadyExpr c i o . mapExpr


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
    processRuntime <- runHawkInterpreter contextDir $ do
      applyContext contextDir
      interpretExpr (runCanonicalExpr e)
    lift $ runHawkIO $ processRuntime hawkRuntime
  where
    hawkRuntime :: HawkRuntime
    hawkRuntime = HawkRuntime i o
