-- | A String-based representation of simple Haskell expressions, typed via phantom types.
module Data.HaskellExpr where

import Text.Printf


newtype HaskellExpr a = HaskellExpr { code :: String }
  deriving (Show, Eq)

($$) :: HaskellExpr (a -> b) -> HaskellExpr a -> HaskellExpr b
HaskellExpr f $$ HaskellExpr x = HaskellExpr $ printf "(%s) (%s)" f x

eLambda :: String -> (HaskellExpr a -> HaskellExpr b) -> HaskellExpr (a -> b)
eLambda var body = HaskellExpr $ printf "\\%s -> %s" var (code eBody)
  where
    eVar = HaskellExpr var
    eBody = body eVar


-- we cannot use any unqualified symbols in the user expression,
-- because we don't know which modules the user prelude will import.
{-# INLINE qualified #-}
qualified :: String -> String -> HaskellExpr a
qualified moduleName unqualifiedName = HaskellExpr qualifiedName
  where
    qualifiedName = printf "%s.%s" moduleName unqualifiedName

{-# INLINE qualifiedInfix #-}
qualifiedInfix :: String -> String
               -> HaskellExpr a -> HaskellExpr b -> HaskellExpr c
qualifiedInfix moduleName unqualifiedName x y = HaskellExpr qualifiedName $$ x $$ y
  where
    qualifiedName = printf "(%s.%s)" moduleName unqualifiedName
