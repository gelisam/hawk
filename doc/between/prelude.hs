{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}
import Prelude
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List as L

between x y = L.takeWhile (/=y) . L.dropWhile (/=x)
