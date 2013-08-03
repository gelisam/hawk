{-# LANGUAGE OverloadedStrings #-}

import           Prelude hiding (break)

import           Data.ByteString.Lazy.Char8 (ByteString, break)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Char
import           Data.List hiding (lines, unlines, break)
import           Data.Maybe
import qualified Data.Map as Map
import           Data.Ord
import           Data.Tuple

import           HSL.Json
import           HSL.Types
import           HSL.Stdlib


%s

f = %s

main = B.getContents >>= mapM_ B.putStrLn . render . f . B.lines
