-- | The fully-qualified HaskellExpr representation of some functions from
--   System.Console.Hawk.Runtime
module System.Console.Hawk.Runtime.HaskellExpr where

import qualified Data.ByteString.Lazy.Char8 as B

import Data.HaskellExpr
import System.Console.Hawk.Runtime.Base


-- The user expression could have any type.
-- If the actual user expression doesn't have the type we use it at, hint
-- will give a type error, and that's fine.
eUserExpression :: String -> HaskellExpr a
eUserExpression = HaskellExpr

-- To avoid an ambiguous type variable, we instantiate the output type of the
-- user expression to unit. The code will still work with any instance of Rows.
eProcessTable :: HaskellExpr (HawkRuntime -> ([[B.ByteString]] -> ()) -> HawkIO ())
eProcessTable = qualified "System.Console.Hawk.Runtime" "processTable"
