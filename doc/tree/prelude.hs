{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
import Prelude
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List as L
import Data.Monoid
import Data.Tree hiding (Forest)
import System.Console.Hawk.Representable


newtype Forest a = Forest {unForest :: [Tree a]}
    deriving Show

instance Functor Forest where
  fmap f = Forest . (fmap.fmap) f . unForest


instance Row a => Rows (Tree a) where
    repr d = display ""
      where
        display indent (Node x ts) = line:lines
          where
            line = indent <> repr' d x
            lines = concatMap (display indent') ts
            indent' = "  " <> indent

instance Row a => Rows (Forest a) where
    repr d (Forest ts) = concatMap (repr d) ts


tree :: Ord p => (a -> p) -> (a -> p) -> [a] -> Forest a
tree pid ppid xs = Forest $ unfoldForest node roots
  where
    roots = filter_ppid (`notElem` map pid xs) xs
    node x = (x, children x)
    children x = filter_ppid (== pid x) xs
    filter_ppid p = filter (p . ppid)
