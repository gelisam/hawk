-- | A wrapper around the hint library, specialized for Hawk usage.
module System.Console.Hawk.Interpreter where

import Control.Monad
import Data.List
import Data.Either
import Data.Function
import Data.Maybe
import Data.String
import Data.Typeable.Internal as Typeable
import Language.Haskell.Interpreter
import System.IO
import Text.Printf (printf)

import Control.Monad.Trans.Uncertain
import System.Console.Hawk.Args
import System.Console.Hawk.Args.Spec
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


runLockedHawkInterpreter :: InterpreterT IO a -> IO (Either InterpreterError a)
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

instance Typeable QualifiedHawkRuntime where
  typeOf (QR bs) = let TypeRep fp tc trs = Typeable.typeOf bs
                   in TypeRep fp
                              tc{ tyConName = "System.Console.Hawk.Runtime.Base."
                                          ++ tyConName tc }
                              trs
