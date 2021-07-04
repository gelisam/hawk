{-# LANGUAGE CPP #-}

-- | Easier access to haskell-src-exts's SrcLoc values.
module Language.Haskell.Exts.Location where

import Control.Monad.Trans.Writer
import Data.Semigroup
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax


-- TODO: haskell-src-exts now supports SrcSpanInfo, which is more informative
-- than SrcLoc, should we switch?

-- | A value obtained from a particular location in the source code.
-- 
-- The location only indicates the beginning of a range, because that's what
-- haskell-src-exts provides.
type Located a = Writer (
#if MIN_VERSION_base(4,11,0)
  Maybe
#else
  Option
#endif
    (Min SrcLoc)) a

located :: SrcLoc -> Located ()
located srcLoc =
  tell $
#if !MIN_VERSION_base(4,11,0)
  Option $
#endif
  if srcLoc == noLoc
  then Nothing
  else Just $ Min srcLoc

annotated :: (Annotated ast, SrcInfo si) => ast si -> Located (ast si)
annotated x = do
    located $ getPointLoc $ ann x
    return x

runLocated :: Located a -> (a, Maybe SrcLoc)
runLocated = go . runWriter
  where
    go (x, p) = (x, fmap getMin $
#if !MIN_VERSION_base(4,11,0)
                  getOption
#endif
                  p)
