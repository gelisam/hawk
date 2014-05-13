-- | The fully-qualified HaskellExpr representation of some functions from base.
module Data.HaskellExpr.Base where

import Data.HaskellExpr


eId :: HaskellExpr (a -> a)
eId = qualified "Prelude" "id"

eConst :: HaskellExpr (a -> b -> a)
eConst = qualified "Prelude" "const"

eFlip :: HaskellExpr ((a -> b -> c) -> b -> a -> c)
eFlip = qualified "Prelude" "flip"

eMap :: HaskellExpr ((a -> b) -> [a] -> [b])
eMap = qualified "Prelude" "map"

eHead :: HaskellExpr ([a] -> a)
eHead = qualified "Prelude" "head"

eComp :: HaskellExpr (b -> c) -> HaskellExpr (a -> b) -> HaskellExpr (a -> c)
eComp = qualifiedInfix "Prelude" "."
