-- | Easier access to haskell-src-exts's SrcLoc values.
module Language.Haskell.Exts.Location where

import Control.Monad
import Control.Monad.Trans.Writer
import Language.Haskell.Exts.Syntax

import Data.Monoid.Ord


-- | Many haskell-src-exts datastructures contain a SrcLoc,
--   this provides a uniform way to access them.
class Location a where
  location :: a -> Maybe SrcLoc

instance Location SrcLoc where
  location = Just

instance Location Module where
  location (Module loc _ _ _ _ _ _) = Just loc

instance Location ModulePragma where
  location (LanguagePragma  loc _)   = Just loc
  location (OptionsPragma   loc _ _) = Just loc
  location (AnnModulePragma loc _)   = Just loc

instance Location ImportDecl where
  location = Just . importLoc

instance Location Decl where
  location (TypeDecl         loc _ _ _)         = Just loc
  location (TypeFamDecl      loc _ _ _)         = Just loc
  location (DataDecl         loc _ _ _ _ _ _)   = Just loc
  location (GDataDecl        loc _ _ _ _ _ _ _) = Just loc
  location (DataFamDecl      loc _ _ _ _)       = Just loc
  location (TypeInsDecl      loc _ _)           = Just loc
  location (DataInsDecl      loc _ _ _ _)       = Just loc
  location (GDataInsDecl     loc _ _ _ _ _)     = Just loc
  location (ClassDecl        loc _ _ _ _ _)     = Just loc
  location (InstDecl         loc _ _ _ _ _ _ )  = Just loc
  location (DerivDecl        loc _ _ _ _ _ )    = Just loc
  location (InfixDecl        loc _ _ _)         = Just loc
  location (DefaultDecl      loc _)             = Just loc
  location (SpliceDecl       loc _)             = Just loc
  location (TypeSig          loc _ _)           = Just loc
  location (FunBind matches) = location matches
  location (PatBind          loc _ _ _)         = Just loc
  location (ForImp           loc _ _ _ _ _)     = Just loc
  location (ForExp           loc _ _ _ _)       = Just loc
  location (RulePragmaDecl   loc _)             = Just loc
  location (DeprPragmaDecl   loc _)             = Just loc
  location (WarnPragmaDecl   loc _)             = Just loc
  location (InlineSig        loc _ _ _)         = Just loc
  location (InlineConlikeSig loc _ _)           = Just loc
  location (SpecSig          loc _ _ _)         = Just loc
  location (SpecInlineSig    loc _ _ _ _)       = Just loc
  location (InstSig          loc _ _ _ _)       = Just loc
  location (AnnPragma        loc _)             = Just loc

instance Location Match where
  location (Match loc _ _ _ _ _) = Just loc

instance Location a => Location (Maybe a) where
  location = join . fmap location

instance Location a => Location [a] where
  -- the earliest location.
  location = getMinPriority . execWriter . mapM_ located


-- | A value obtained from a particular location in the source code.
-- 
-- The location only indicates the beginning of a range, because that's what
-- haskell-src-exts provides.
type Located a = Writer (MinPriority SrcLoc) a

located :: Location a => a -> Located a
located x = do
    tell $ MinPriority $ location x
    return x

runLocated :: Located a -> (a, Maybe SrcLoc)
runLocated = go . runWriter
  where
    go (x, p) = (x, getMinPriority p)
