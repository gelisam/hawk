-- | In which the user prelude is massaged into the form hint needs.
module System.Console.Hawk.UserPrelude where

import Data.ByteString as B
import Text.Printf

import Control.Monad.Trans.Uncertain
import Data.HaskellModule
import System.Console.Hawk.UserPrelude.Extend


type UserPrelude = HaskellModule


testC :: FilePath -> IO ()
testC f = do
    let orig = printf "tests/preludes/%s/prelude.hs" f
    m <- runUncertainIO $ readModule orig
    B.putStr $ showModule orig (canonicalUserPrelude m)

-- |
-- >>> testC "default"
-- {-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}
-- import Prelude
-- import qualified Data.ByteString.Lazy.Char8 as B
-- import qualified Data.List as L
-- 
-- >>> testC "moduleName"
-- module MyPrelude where
-- import Prelude
-- {-# LINE 2 "tests/preludes/moduleName/prelude.hs" #-}
-- t = take
canonicalUserPrelude :: HaskellModule -> UserPrelude
canonicalUserPrelude = extendImports

compileUserPrelude :: FilePath -> IO ()
compileUserPrelude = undefined
