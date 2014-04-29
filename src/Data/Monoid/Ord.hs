-- based on http://hackage.haskell.org/package/monoids-0.3.2/docs/src/Data-Monoid-Ord.html
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
---- |
---- Module      :  Data.Monoid.Ord
---- Copyright   :  (c) Edward Kmett 2009
---- License     :  BSD-style
---- Maintainer  :  ekmett@gmail.com
---- Stability   :  experimental
---- Portability :  portable
----
---- Some 'Monoid' instances that should probably be in "Data.Monoid".
----
-----------------------------------------------------------------------------

module Data.Monoid.Ord 
    (
    -- * Max
      Max(Max,getMax)
    -- * Min
    , Min(Min,getMin)
    -- * MaxPriority: Max semigroup w/ added bottom
    , MaxPriority(MaxPriority,getMaxPriority)
    , minfinity
    -- * MinPriority: Min semigroup w/ added top
    , MinPriority(MinPriority,getMinPriority)
    , infinity
    ) where

import Data.Monoid (Monoid, mappend, mempty)

-- | The 'Monoid' @('max','minBound')@
newtype Max a = Max { getMax :: a } deriving (Eq,Ord,Show,Read,Bounded)

instance (Ord a, Bounded a) => Monoid (Max a) where
    mempty = Max minBound
    mappend = max

instance Functor Max where 
    fmap f (Max a) = Max (f a)

-- | The 'Monoid' given by @('min','maxBound')@
newtype Min a = Min { getMin :: a } deriving (Eq,Ord,Show,Read,Bounded)

instance (Ord a, Bounded a) => Monoid (Min a) where
    mempty = Min maxBound
    mappend = min

instance Functor Min where
    fmap f (Min a) = Min (f a)

minfinity :: MaxPriority a
minfinity = MaxPriority Nothing

-- | The 'Monoid' @('max','Nothing')@ over @'Maybe' a@ where 'Nothing' is the bottom element
newtype MaxPriority a = MaxPriority { getMaxPriority :: Maybe a } deriving (Eq,Ord,Show,Read)

instance Ord a => Monoid (MaxPriority a) where
    mempty = MaxPriority Nothing
    mappend = max

instance Functor MaxPriority where
    fmap f (MaxPriority a) = MaxPriority (fmap f a)

infinity :: MinPriority a
infinity = MinPriority Nothing

-- | The 'Monoid' @('min','Nothing')@ over @'Maybe' a@ where 'Nothing' is the top element
newtype MinPriority a = MinPriority { getMinPriority :: Maybe a } deriving (Eq,Show,Read)

instance Ord a => Ord (MinPriority a) where
    MinPriority Nothing  `compare` MinPriority Nothing  = EQ
    MinPriority Nothing  `compare` _                    = GT
    _                    `compare` MinPriority Nothing  = LT
    MinPriority (Just a) `compare` MinPriority (Just b) = a `compare` b

instance Ord a => Monoid (MinPriority a) where
    mempty = MinPriority Nothing
    mappend = min

instance Functor MinPriority where
    fmap f (MinPriority a) = MinPriority (fmap f a)
