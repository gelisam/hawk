-- | The fully-qualified HaskellExpr representation of some functions from
--   System.Console.Hawk.Runtime
module Data.HaskellExpr.Runtime where

import qualified Data.ByteString.Lazy.Char8 as B

import Data.HaskellExpr
import System.Console.Hawk.Runtime.Base


-- To avoid an ambiguous type variable, we instantiate the output type of the user
-- expression to unit. The code will still work with any instance of Rows.
eUserExpression :: String -> HaskellExpr ([[B.ByteString]] -> ())
eUserExpression = HaskellExpr

eProcessTable :: HaskellExpr (HawkRuntime -> ([[B.ByteString]] -> ()) -> HawkIO ())
eProcessTable = qualified "System.Console.Hawk.Runtime" "processTable"
