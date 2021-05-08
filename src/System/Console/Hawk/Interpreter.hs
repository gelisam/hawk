{-# LANGUAGE CPP #-}
-- | A wrapper around the hint library, specialized for Hawk usage.
module System.Console.Hawk.Interpreter
  ( applyContext
  , runHawkInterpreter
  ) where

import Prelude hiding (fail)

import Control.Monad hiding (fail)
#if MIN_VERSION_base(4,12,0)
import Control.Monad.Fail (fail)
#else
import Prelude (fail)
#endif
import Data.List
import Language.Haskell.Interpreter

import Control.Monad.Trans.Uncertain
import qualified System.Console.Hawk.Context as Context
import qualified System.Console.Hawk.Context.Dir as Context
import qualified System.Console.Hawk.PackageDbs as PackageDbs
import System.Console.Hawk.UserPrelude.Defaults
import System.Console.Hawk.Lock

-- $setup
-- >>> import System.Console.Hawk.Args.Spec


-- | Tell hint to load the user prelude, the modules it imports, and the
--   language extensions it specifies.
applyContext :: FilePath -- ^ context directory
             -> InterpreterT IO ()
applyContext contextDir = do
    context <- lift $ runUncertainIO $ Context.getContext contextDir
    
    let extensions = map read $ Context.extensions context
    let preludeFile = Context.canonicalPreludePath (Context.contextPaths context)
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


runHawkInterpreter :: FilePath -> InterpreterT IO a -> UncertainT IO a
runHawkInterpreter cxtDir body = do
  Context.createDefaultContextDir cxtDir
  wrapErrorsM . withLock cxtDir . PackageDbs.runHawkInterpreter $ body
