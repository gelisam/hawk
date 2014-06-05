{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}
import Prelude
import qualified Data.List as L
import qualified Data.Text.Lazy as T
takeLast n = reverse . take n . reverse
