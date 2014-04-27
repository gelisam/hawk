-- | The names of the important files inside the context directory.
module System.Console.Hawk.Context.Paths
  ( ContextPaths  -- the type, not the constructor
  , originalPreludePath, cacheDirPath
  , canonicalPreludePath, compiledPreludePath, cachedPreludePath
  , mkContextPaths
  ) where

import System.FilePath


data ContextPaths = ContextPaths
  { originalPreludePath :: FilePath
  , cacheDirPath :: FilePath
  , canonicalPreludePath :: FilePath
  , compiledPreludePath :: FilePath
  , cachedPreludePath :: FilePath
  } deriving (Eq, Read, Show)

mkContextPaths :: FilePath -> ContextPaths
mkContextPaths confDir = ContextPaths
    { originalPreludePath  = confDir </> "prelude.hs"
    , cacheDirPath         = cacheDir
    , canonicalPreludePath = cacheDir </> "cached_prelude.hs"
    , compiledPreludePath  = cacheDir </> "cached_prelude.o"
    , cachedPreludePath    = cacheDir </> "cached_prelude.dat"
    }
  where
    cacheDir = confDir </> "cache"
