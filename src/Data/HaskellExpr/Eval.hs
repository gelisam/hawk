{-# LANGUAGE ScopedTypeVariables #-}
-- | Evaluate an HaskellExpr using the hint library.
module Data.HaskellExpr.Eval where

import Data.Typeable
import Language.Haskell.Interpreter

import Data.HaskellExpr


interpretExpr :: forall m a. (MonadInterpreter m, Typeable a)
              => HaskellExpr a -> m a
interpretExpr eExpr = interpret (code eExpr) (as :: a)
