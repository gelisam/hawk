{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}

import           Prelude
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.List as L
import           Data.Monoid

postorder :: (ByteString -> [a] -> a) -> [ByteString] -> [a]
postorder call [] = []
postorder call (f:xs) = call f' ys
                      : postorder call xs'
  where
    n = indent f
    f' = B.drop n f
    ys = postorder call block
    
    indent = B.length . B.takeWhile (==' ')
    not_indented x = indent x <= n
    (block, xs') = break not_indented xs
