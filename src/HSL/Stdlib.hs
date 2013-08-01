
module HSL.Stdlib where

import           Data.Ord (comparing)
import           Data.List (groupBy, sortBy)
import qualified Data.Map as Map
import           Data.Tuple (swap)


groupOn :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy (\a b -> f a == f b)


sortOn :: (Ord b) => (a -> b) -> [a] -> [a]
sortOn f = sortBy (\a b -> compare (f a) (f b))


hist :: (Ord a) => [a] -> [(Int, a)]
hist = sortOn fst . map swap . Map.toList . foldl inc Map.empty
  where inc = (\m v -> Map.insertWith (+) v 1 m)
