{-# LANGUAGE OverloadedStrings #-}

import           Prelude hiding (break)

import           Control.Category ((>>>))
import           Control.Lens
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

default (Int, Double)


%s

p = %s

run :: (ParsableFromFile a, RenderableFile b) => (a -> b) -> IO ()
run f = B.getContents >>= B.putStr . renderFile . f . parseFile

main = run p
