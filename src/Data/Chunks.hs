-- | For dividing a list into sublists.
module Data.Chunks where

import Data.Monoid
import Data.Foldable
import Data.Sequence hiding (empty, singleton)
import qualified Data.Sequence as S


-- | Isomorphic to [[a]], only the instances are different.
type Chunks a = StickyList [a]

fromLists :: [[a]] -> Chunks a
fromLists = StickyList . fromList

-- |
-- >>> toLists (fromLists ["foo", "bar"] <> fromLists ["baz", "quux"])
-- ["foo","barbaz","quux"]
-- 
-- >>> toLists (singleton "foo" <> empty <> singleton "bar" <> divider <> singleton "baz" <> singleton "quux")
-- ["foobar","bazquux"]
toLists :: Chunks a -> [[a]]
toLists = toList . runStickyList


empty :: StickyList a
empty = StickyList S.empty

singleton :: a -> StickyList a
singleton = StickyList . S.singleton

divider :: Monoid a => StickyList a
divider = StickyList (S.singleton mempty >< S.singleton mempty)


-- | Generalize Chunks to arbitrary monoids.
--
-- Appending two sticky lists causes the two endpoints to stick together
-- via mappend.
newtype StickyList w = StickyList { runStickyList :: Seq w }

instance Monoid w => Monoid (StickyList w) where
  mempty = StickyList S.empty
  mappend (StickyList xs') (StickyList ys') | S.null xs' = StickyList ys'
  mappend (StickyList xs') (StickyList ys') | S.null ys' = StickyList xs'
  mappend (StickyList xs') (StickyList ys') = StickyList zs'
    where
      (xs :> x) = viewr xs'
      (y :< ys) = viewl ys'
      z = x `mappend` y
      zs' = xs >< S.singleton z >< ys
