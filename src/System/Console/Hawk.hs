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

{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , ScopedTypeVariables
           , TupleSections #-}
-- | Hawk as seen from the outside world: parsing command-line arguments,
--   evaluating user expressions.
module System.Console.Hawk
  ( processArgs
  ) where


import Control.Monad
import qualified Data.List as L
import Data.List ((++))
import Data.Either
import Data.Function
import Data.Maybe
import Data.String
import qualified Data.Typeable.Internal as Typeable
import Data.Typeable.Internal
  (TypeRep(..)
  ,tyConName)
import Language.Haskell.Interpreter
import qualified Prelude as P
import qualified System.IO as IO
import System.IO (IO)
import Text.Printf (printf)

import Control.Monad.Trans.Uncertain
import System.Console.Hawk.Args
import System.Console.Hawk.Args.Compatibility
import System.Console.Hawk.Args.Spec
import qualified System.Console.Hawk.Context as Context
import System.Console.Hawk.Context.Compatibility
import System.Console.Hawk.Sandbox
import System.Console.Hawk.UserPrelude
import System.Console.Hawk.Help
import System.Console.Hawk.Lock
import System.Console.Hawk.Runtime.Base
import System.Console.Hawk.Version


-- | Tell hint to load the user prelude, the modules it imports, and the
--   language extensions it specifies.
initInterpreter :: (String, String) -- ^ prelude file and module name
                -> [(String,Maybe String)] -- ^ the modules maybe qualified
                -> [Extension]
                -> InterpreterT IO ()
initInterpreter (preludeFile,preludeModule) userModules extensions = do
        
        set [languageExtensions := extensions]

        -- load the prelude file
        loadModules [preludeFile]

        -- load the prelude module plus representable
        setImportsQ $ (preludeModule,Nothing):defaultModules
                                           ++ userModules


errorString :: InterpreterError -> String
errorString (WontCompile es) = L.intercalate "\n" (header : P.map indent es)
  where
    header = "Won't compile:"
    indent (GhcError e) = ('\t':e)
errorString e = P.show e

wrapErrorsM :: Monad m => m (Either InterpreterError a) -> UncertainT m a
wrapErrorsM = lift >=> wrapErrors

wrapErrors :: Monad m => Either InterpreterError a -> UncertainT m a
wrapErrors (Left e) = fail $ errorString e
wrapErrors (Right x) = return x


runLockedHawkInterpreter :: forall a . InterpreterT IO a
                            -> IO (Either InterpreterError a)
runLockedHawkInterpreter i = withLock $ runHawkInterpreter i

-- | Wrapper used to force `typeOf` to fully-qualify the type
--   `HawkRuntime`. Otherwise hint may try to use a type which
--   we haven't explicitly imported.
-- 
-- >>> let runtime = HawkRuntime defaultInputSpec defaultOutputSpec
-- 
-- >>> Typeable.typeOf runtime
-- HawkRuntime
-- 
-- >>> Typeable.typeOf $ QR runtime
-- System.Console.Hawk.Runtime.Base.HawkRuntime
newtype QualifiedHawkRuntime = QR HawkRuntime

instance Typeable.Typeable QualifiedHawkRuntime where
  typeOf (QR bs) = let TypeRep fp tc trs = Typeable.typeOf bs
                   in TypeRep fp
                              tc{ tyConName = "System.Console.Hawk.Runtime.Base."
                                          ++ tyConName tc }
                              trs


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
processSpec Version       = IO.putStrLn versionString
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
    
    context <- Context.getContext contextDir
    
    let prelude = configFromContext context
    
    let extensions = P.map P.read $ Context.extensions context
    let modules = Context.modules context
    let expr = userExpression e

    processRuntime <- runUncertainIO
                    $ wrapErrorsM
                    $ runLockedHawkInterpreter
                    $ do
      initInterpreter prelude modules extensions
      interpret' $ processTable $ tableExpr expr
    processRuntime (QR hawkRuntime)
  where
    interpret' expr = do
      interpret expr (as :: QualifiedHawkRuntime -> IO ())
    
    hawkRuntime = HawkRuntime i o
    
    processTable :: String -> String
    processTable = printf "(%s) (%s) (%s)" (prel "flip")
                                           (runtime "processTable")
    
    -- turn the user into an expression manipulating [[B.ByteString]]
    tableExpr :: String -> String
    tableExpr e = e `compose` fromTable
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
    runtime = qualify "System.Console.Hawk.Runtime.Base"
