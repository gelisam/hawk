-- | In which the user prelude is massaged into the form hint needs.
module System.Console.Hawk.UserPrelude where

import           Control.Monad.Trans.Class
import           Data.ByteString as B
import           Text.Printf

import           Control.Monad.Trans.Uncertain
import           Data.HaskellModule
import           System.Console.Hawk.Sandbox
import           System.Console.Hawk.UserPrelude.Extend


type UserPrelude = HaskellModule


testC :: FilePath -> IO ()
testC f = do
    let orig = printf "tests/preludes/%s/prelude.hs" f
    m <- runUncertainIO $ readModule orig
    B.putStr $ showModule orig (canonicalizeUserPrelude m)

-- |
-- >>> testC "default"
-- {-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}
-- module System.Console.Hawk.CachedPrelude where
-- {-# LINE 2 "tests/preludes/default/prelude.hs" #-}
-- import Prelude
-- import qualified Data.ByteString.Lazy.Char8 as B
-- import qualified Data.List as L
--
-- >>> testC "moduleName"
-- module MyPrelude where
-- import Prelude
-- {-# LINE 2 "tests/preludes/moduleName/prelude.hs" #-}
-- t = take
canonicalizeUserPrelude :: HaskellModule -> UserPrelude
canonicalizeUserPrelude = extendModuleName . extendImports

readUserPrelude :: FilePath -> UncertainT IO UserPrelude
readUserPrelude f = canonicalizeUserPrelude <$> readModule f


compileUserPrelude :: FilePath -- ^ the original's filename,
                               --   used for fixing up line numbers
                   -> FilePath -- ^ new filename, because ghc compiles from disk.
                               --   the compiled output will be in the same folder.
                   -> UserPrelude
                   -> UncertainT IO ()
compileUserPrelude = compileUserPreludeWithArgs []

compileUserPreludeWithArgs :: [String] -- ^ extra ghc args
                           -> FilePath -- ^ the original's filename,
                                       --   used for fixing up line numbers
                           -> FilePath -- ^ new filename, because ghc compiles from disk.
                                       --   the compiled output will be in the same folder.
                           -> UserPrelude
                           -> UncertainT IO ()
compileUserPreludeWithArgs args orig f m = do
    extraArgs <- lift $ extraGhcArgs
    let args' = (extraArgs ++ args)
    compileModuleWithArgs args' orig f m
