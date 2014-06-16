{-# LANGUAGE ScopedTypeVariables #-}
-- | Evaluate an HaskellExpr using the hint library.
module Data.HaskellExpr.Eval where

import Data.Typeable
import Language.Haskell.Interpreter

import Data.HaskellExpr


interpretExpr :: (Typeable a, MonadInterpreter m)
              => HaskellExpr a -> m a
interpretExpr eExpr = interpret (code eExpr) proxy
  where
    proxy :: a
    proxy = undefined
