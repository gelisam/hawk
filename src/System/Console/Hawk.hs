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


import Language.Haskell.Interpreter
import Text.Printf (printf)

import Control.Monad.Trans.Uncertain
import System.Console.Hawk.Args
import System.Console.Hawk.Args.Spec
import System.Console.Hawk.Help
import System.Console.Hawk.Interpreter
import System.Console.Hawk.Runtime.Base
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
processSpec (Eval  e   o) = applyExpr (wrapExpr "const" e) noInput o
processSpec (Apply e i o) = applyExpr e                    i       o
processSpec (Map   e i o) = applyExpr (wrapExpr "map"   e) i       o

wrapExpr :: String -> ExprSpec -> ExprSpec
wrapExpr f e = e'
  where
    u = userExpression e
    u' = printf "%s (%s)" (prel f) u
    e' = e { userExpression = u' }
    
    -- we cannot use any unqualified symbols in the user expression,
    -- because we don't know which modules the user prelude will import.
    qualify :: String -> String -> String
    qualify moduleName = printf "%s.%s" moduleName
    
    prel = qualify "Prelude"

applyExpr :: ExprSpec -> InputSpec -> OutputSpec -> IO ()
applyExpr e i o = do
    let contextDir = userContextDirectory e
    let expr = userExpression e
    
    processRuntime <- runUncertainIO $ runHawkInterpreter $ do
      applyContext contextDir
      interpret' $ processTable' $ tableExpr expr
    runHawkIO $ processRuntime hawkRuntime
  where
    interpret' expr = do
      interpret expr (as :: HawkRuntime -> HawkIO ())
    
    hawkRuntime = HawkRuntime i o
    
    processTable' :: String -> String
    processTable' = printf "(%s) (%s) (%s)" (prel "flip")
                                            (runtime "processTable")
    
    -- turn the user expr into an expression manipulating [[B.ByteString]]
    tableExpr :: String -> String
    tableExpr = (`compose` fromTable)
      where
        fromTable = case inputFormat i of
            RawStream         -> head' `compose` head'
            Lines _ RawLine   -> map' head'
            Lines _ (Words _) -> prel "id"
    
    compose :: String -> String -> String
    compose f g = printf "(%s) %s (%s)" f (prel ".") g
    
    head' :: String
    head' = prel "head"
    
    map' :: String -> String
    map' = printf "(%s) (%s)" (prel "map")
    
    -- we cannot use any unqualified symbols in the user expression,
    -- because we don't know which modules the user prelude will import.
    qualify :: String -> String -> String
    qualify moduleName = printf "%s.%s" moduleName
    
    prel = qualify "Prelude"
    runtime = qualify "System.Console.Hawk.Runtime"
