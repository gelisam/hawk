{-# LANGUAGE RecordWildCards #-}
-- | An opaque representation of a Haskell module.
-- 
-- The few parts which can be modified are easier to modify than using raw
-- bytes or a raw HaskellSource.
module Data.HaskellModule
  ( HaskellModule
  , emptyModule, addExtension, addImport
  , readModule
  , printModule, writeModule
  ) where

import qualified Data.ByteString.Char8 as B

import Data.HaskellSource

-- Most of the API is re-exported from those submodules
import Data.HaskellModule.Base
import Data.HaskellModule.Parse


-- |
-- >>> B.putStr $ printModule "orig.hs" $ emptyModule
-- 
-- >>> B.putStr $ printModule "orig.hs" $ addExtension "OverloadedStrings" $ addImport ("Data.ByteString.Char8", Just "B") $ addExtension "RecordWildCards" $ addImport ("Prelude", Nothing) $ emptyModule
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RecordWildCards #-}
-- import qualified Data.ByteString.Char8 as B
-- import Prelude
printModule :: FilePath -- ^ the original's filename,
                        --   used for fixing up line numbers
            -> HaskellModule -> B.ByteString
printModule orig (HaskellModule {..}) = printSource orig fullSource
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
writeModule orig f = B.writeFile f . printModule orig
