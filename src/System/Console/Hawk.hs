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


import qualified Data.ByteString.Lazy.Char8 as B

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Uncertain
import Data.HaskellExpr
import Data.HaskellExpr.Base
import Data.HaskellExpr.Eval
import System.Console.Hawk.Args
import System.Console.Hawk.Args.Spec
import System.Console.Hawk.Help
import System.Console.Hawk.Interpreter
import System.Console.Hawk.Runtime
import System.Console.Hawk.Runtime.Base
import System.Console.Hawk.Runtime.HaskellExpr
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
processSpec (Eval  e   o) = runEvalSpec  (contextSpec e)   o (userExpression e)
processSpec (Apply e i o) = runApplySpec (contextSpec e) i o (userExpression e)
processSpec (Map   e i o) = runMapSpec   (contextSpec e) i o (userExpression e)

userExpression :: ExprSpec -> OriginalUserExpression
userExpression = originalUserExpression . untypedUserExpression


-- | While the original user input may describe a value or a function on a
--   single record, the processed user expression is always a function on
--   the entire input. Also, its output is wrapped in `SomeRows`, to make
--   sure we don't accidentally rely on the fake `()` return type used by
--   `OriginalUserExpression`.
type ProcessedUserExpression = UserExpression (() -> SomeRows)
                                              (B.ByteString -> SomeRows)
                                              ([B.ByteString] -> SomeRows)
                                              ([[B.ByteString]] -> SomeRows)

-- | Asserts that the user expression is not a function, and applies `const`
--   to it in order to make it a function.
constExpression :: OriginalUserExpression -> ProcessedUserExpression
constExpression (UserExpression e _ _ _) = UserExpression (eAp eConst . eAp eSomeRows <$> e)
                                                          (eAp eConst . eAp eSomeRows <$> e)
                                                          (eAp eConst . eAp eSomeRows <$> e)
                                                          (eAp eConst . eAp eSomeRows <$> e)

-- | Asserts that the user expression is a function.
applyExpression :: OriginalUserExpression -> ProcessedUserExpression
applyExpression (UserExpression _ e1 e2 e3) = UserExpression Nothing
                                                             (eComp eSomeRows <$> e1)
                                                             (eComp eSomeRows <$> e2)
                                                             (eComp eSomeRows <$> e3)

-- | Asserts that the user expression is a function on one record, and applies
--   `map` to it in order to make it a function on all records.
mapExpression :: OriginalUserExpression -> ProcessedUserExpression
mapExpression (UserExpression _ e1 e2 _) = UserExpression Nothing
                                                          Nothing
                                                          (eComp eSomeRows . eAp eMap <$> e1)
                                                          (eComp eSomeRows . eAp eMap <$> e2)


-- | Regardless of the requested input format, we currently convert all user expressions
--   so that they expect a `[[ByteString]]`. The runtime will also look at the input format,
--   in order to encode it as a `[[ByteString]]` as well. For example, if the input format is
--   supposed to be a single `ByteString`, the runtime will pack that string `s` into a nested
--   singleton list `[[s]]`, and the canonical user expression will expect this format.
type CanonicalUserExpression = HaskellExpr ([[B.ByteString]] -> SomeRows)

-- Could fail if the required case of the user expression is `Nothing`, meaning that the
-- other flags were not compatible with the requested input format.
canonicalizeExpression :: InputSpec -> ProcessedUserExpression -> Maybe CanonicalUserExpression
canonicalizeExpression i (UserExpression _ e1 e2 e3) = case inputFormat i of
  RawStream            -> (`eComp` (eHead `eComp` eHead)) <$> e1
  Records _ RawRecord  -> (`eComp` (eMap `eAp` eHead)) <$> e2
  Records _ (Fields _) -> e3


runEvalSpec :: ContextSpec -> OutputSpec -> OriginalUserExpression -> IO ()
runEvalSpec c o = runUncertainIO . runProcessedExpr c noInput o . constExpression

runApplySpec :: ContextSpec -> InputSpec -> OutputSpec -> OriginalUserExpression -> IO ()
runApplySpec c i o = runUncertainIO . runProcessedExpr c i o . applyExpression

runMapSpec :: ContextSpec -> InputSpec -> OutputSpec -> OriginalUserExpression -> IO ()
runMapSpec c i o = runUncertainIO . runProcessedExpr c i o . mapExpression


runProcessedExpr :: ContextSpec
                 -> InputSpec
                 -> OutputSpec
                 -> ProcessedUserExpression
                 -> UncertainT IO ()
runProcessedExpr c i o e = case canonicalizeExpression i e of
    Just e' -> runCanonicalExpr c i o e'
    Nothing -> fail "conflicting flags"

runCanonicalExpr :: ContextSpec
                 -> InputSpec
                 -> OutputSpec
                 -> CanonicalUserExpression
                 -> UncertainT IO ()
runCanonicalExpr c i o e = do
    let contextDir = userContextDirectory c
    processRuntime <- runHawkInterpreter $ do
      applyContext contextDir
      interpretExpr (eApplyCanonicalExpr e)
    lift $ runHawkIO $ processRuntime hawkRuntime
  where
    hawkRuntime :: HawkRuntime
    hawkRuntime = HawkRuntime i o
    
    eApplyCanonicalExpr :: CanonicalUserExpression -> HaskellExpr (HawkRuntime -> HawkIO ())
    eApplyCanonicalExpr eExpr = eFlip `eAp` eProcessTable `eAp` eExpr
