{-# LANGUAGE PackageImports #-}
-- | Everything we need to know in order to evaluate a user expression,
--   except for the user expression itself.
module System.Console.Hawk.Context.Base
  ( Context(..)
  , getContext
  ) where

import "mtl" Control.Monad.Trans
import System.Directory
import System.IO

import Control.Monad.Trans.Uncertain
import Control.Monad.Trans.State.Persistent
import Data.Cache
import System.Console.Hawk.Context.Dir
import System.Console.Hawk.Lock
import System.Console.Hawk.UserPrelude
import System.Console.Hawk.UserPrelude.Base
import System.Console.Hawk.UserPrelude.Cache
import System.Console.Hawk.UserPrelude.Parse


data Context = Context
  { originalPreludePath :: FilePath
  , canonicalPrelude :: FilePath
  , compiledPrelude :: FilePath
  , moduleName :: String
  , extensions :: [ExtensionName]
  , modules :: [QualifiedModule]
  } deriving (Eq, Read, Show)

-- | Obtains a Context, either from the cache or from the user prelude.
getContext :: FilePath -> IO Context
getContext confDir = do
    runUncertainIO $ createDefaultContextDir confDir
    -- skip `newContext` if the cached copy is still good.
    let preludeFile = getUserPreludeFile confDir
    let cacheFile   = getContextFile confDir
    key <- getKey preludeFile
    let cache = singletonCache assocCache
    withLock $ withPersistentStateT cacheFile []
             $ cached cache key
             $ lift
             $ newContext confDir
  where
    getKey f = do
        modifiedTime <- getModificationTime f
        fileSize <- withFile f ReadMode hFileSize
        return (f, modifiedTime, fileSize)

-- | Construct a Context by parsing the user prelude.
newContext :: FilePath -> IO Context
newContext confDir = do
    let originalPreludePath' = getUserPreludeFile confDir
    
    (canonicalPrelude', moduleName') <- recompileUserPrelude confDir
    extensions' <- readExtensions originalPreludePath'
    modules' <- readModules extensions' originalPreludePath'
    
    -- I think it hint will automatically use the version we have just
    -- compiled if we give it the path to the .hs file.
    -- 
    -- TODO: check whether using .o or .hi instead works
    -- and whether it makes any difference.
    let compiledPrelude' = canonicalPrelude'
    
    return $ Context
           { originalPreludePath = originalPreludePath'
           , canonicalPrelude = canonicalPrelude'
           , compiledPrelude = compiledPrelude'
           , moduleName = moduleName'
           , extensions = extensions'
           , modules = modules'
           }
