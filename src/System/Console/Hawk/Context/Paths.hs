-- | The names of the important files inside the context directory.
module System.Console.Hawk.Context.Paths
  ( ContextPaths  -- the type, not the constructor
  , contextDirPath, originalPreludePath
  , cacheDirPath, canonicalPreludePath, compiledPreludePath, cachedPreludePath
  , mkContextPaths
  ) where

import System.FilePath


data ContextPaths = ContextPaths
  { contextDirPath :: FilePath
  , originalPreludePath :: FilePath
  , cacheDirPath :: FilePath
  , canonicalPreludePath :: FilePath
  , compiledPreludePath :: FilePath
  , cachedPreludePath :: FilePath
  } deriving (Eq, Read, Show)

-- TODO: if we're always using the same relative paths, we could make illegal
-- states unrepresentable by making ContextPaths = contextDir and turning the
-- accessors into functions
mkContextPaths :: FilePath -> ContextPaths
mkContextPaths contextDir = ContextPaths
    { contextDirPath       = contextDir
    , originalPreludePath  = contextDir </> "prelude.hs"
    , cacheDirPath         = cacheDir
    , canonicalPreludePath = cacheDir </> "cached_prelude.hs"
    , compiledPreludePath  = cacheDir </> "cached_prelude.o"
    , cachedPreludePath    = cacheDir </> "cached_prelude.dat"
    }
  where
    cacheDir = contextDir </> "cache"
