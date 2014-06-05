{-# LANGUAGE RecordWildCards #-}
-- | An opaque representation of a Haskell module.
-- 
-- The few parts which can be modified are easier to modify than using raw
-- bytes or a raw HaskellSource.
module Data.HaskellModule
  ( module Data.HaskellModule.Base
  , module Data.HaskellModule.Parse
  , showModule, writeModule
  , compileModule, compileModuleWithArgs
  ) where

import Control.Monad.Trans.Class
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TextIO

import Data.HaskellSource
import Control.Monad.Trans.Uncertain

-- Most of the API is re-exported from those submodules
import Data.HaskellModule.Base
import Data.HaskellModule.Parse


-- $setup
-- The code examples in this module assume the use of GHC's `OverloadedStrings`
-- extension:
--
-- >>> :set -XOverloadedStrings

-- |
-- >>> TextIO.putStr $ showModule "orig.hs" $ emptyModule
--
-- >>> TextIO.putStr $ showModule "orig.hs" $ addExtension "OverloadedStrings" $ addExtension "RecordWildCards" $ addImport ("Prelude", Nothing) $ emptyModule
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RecordWildCards #-}
-- import Prelude
showModule :: FilePath -- ^ the original's filename,
                       --   used for fixing up line numbers
           -> HaskellModule -> T.Text
showModule orig (HaskellModule {..}) = showSource orig fullSource
  where
    fullSource = concat [ pragmaSource
                        , moduleSource
                        , importSource
                        , codeSource
                        ]

writeModule :: FilePath -- ^ the original's filename,
                        --   used for fixing up line numbers
            -> FilePath
            -> HaskellModule
            -> IO ()
writeModule orig f = TextIO.writeFile f . showModule orig


compileModule :: FilePath -- ^ the original's filename,
                          --   used for fixing up line numbers
              -> FilePath -- ^ new filename, because ghc compiles from disk.
                          --   the compiled output will be in the same folder.
              -> HaskellModule
              -> UncertainT IO ()
compileModule = compileModuleWithArgs []

compileModuleWithArgs :: [String] -- ^ extra ghc args
                      -> FilePath -- ^ the original's filename,
                                  --   used for fixing up line numbers
                      -> FilePath -- ^ new filename, because ghc compiles from disk.
                                  --   the compiled output will be in the same folder.
                      -> HaskellModule
                      -> UncertainT IO ()
compileModuleWithArgs args orig f m = do
    lift $ writeModule orig f m
    compileFileWithArgs args f
