-- | A wrapper around the hint library, specialized for Hawk usage.
module System.Console.Hawk.Interpreter
  ( QualifiedHawkRuntime(..)
  , initInterpreter
  , runHawkInterpreter
  ) where

import Control.Monad
import Data.List
import Data.Typeable.Internal as Typeable
import Language.Haskell.Interpreter

import Control.Monad.Trans.Uncertain
import qualified System.Console.Hawk.Context as Context
import qualified System.Console.Hawk.Sandbox as Sandbox
import System.Console.Hawk.UserPrelude
import System.Console.Hawk.Lock
import System.Console.Hawk.Runtime.Base

-- $setup
-- >>> import System.Console.Hawk.Args.Spec


-- | Tell hint to load the user prelude, the modules it imports, and the
--   language extensions it specifies.
initInterpreter :: FilePath -- ^ context directory
                -> InterpreterT IO ()
initInterpreter contextDir = do
    context <- lift $ Context.getContext contextDir
    
    let extensions = map read $ Context.extensions context
    let preludeFile = Context.canonicalPrelude context
    let preludeModule = Context.moduleName context
    let userModules = Context.modules context
    
    set [languageExtensions := extensions]
    
    -- load the prelude file
    loadModules [preludeFile]
    
    -- load the prelude module plus representable etc.
    setImportsQ $ (preludeModule,Nothing):defaultModules
                                       ++ userModules


errorString :: InterpreterError -> String
errorString (WontCompile es) = intercalate "\n" (header : map indent es)
  where
    header = "Won't compile:"
    indent (GhcError e) = ('\t':e)
errorString e = show e

wrapErrorsM :: Monad m => m (Either InterpreterError a) -> UncertainT m a
wrapErrorsM = lift >=> wrapErrors

wrapErrors :: Monad m => Either InterpreterError a -> UncertainT m a
wrapErrors (Left e) = fail $ errorString e
wrapErrors (Right x) = return x


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

instance Typeable QualifiedHawkRuntime where
  typeOf (QR bs) = let TypeRep fp tc trs = Typeable.typeOf bs
                   in TypeRep fp
                              tc{ tyConName = "System.Console.Hawk.Runtime.Base."
                                          ++ tyConName tc }
                              trs


runHawkInterpreter :: InterpreterT IO a -> UncertainT IO a
runHawkInterpreter = wrapErrorsM . withLock . Sandbox.runHawkInterpreter
