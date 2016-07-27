-- | The fully-qualified HaskellExpr representation of some functions from
--   System.Console.Hawk.Runtime
module System.Console.Hawk.Runtime.HaskellExpr where

import qualified Data.ByteString.Lazy.Char8       as B

import           Data.HaskellExpr
import           System.Console.Hawk.Runtime
import           System.Console.Hawk.Runtime.Base


eSomeRows :: HaskellExpr (a -> SomeRows)
eSomeRows = qualified "System.Console.Hawk.Runtime" "SomeRows"

eProcessTable :: HaskellExpr (HawkRuntime -> ([[B.ByteString]] -> SomeRows) -> HawkIO ())
eProcessTable = qualified "System.Console.Hawk.Runtime" "processTable"
