{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}

import           Prelude
import qualified Data.List as L
import qualified Data.Text.Lazy as T
import           Data.Text.Format

postorder :: (T.Text -> [a] -> a) -> [T.Text] -> [a]
postorder call [] = []
postorder call (f:xs) = call f' ys
                      : postorder call xs'
  where
    n = indent f
    f' = T.drop n f
    ys = postorder call block

    indent = T.length . T.takeWhile (==' ')
    not_indented x = indent x <= n
    (block, xs') = break not_indented xs
