{-# LANGUAGE PackageImports #-}
-- | Everything we need to know in order to evaluate a user expression,
--   except for the user expression itself.
module System.Console.Hawk.Context.Base
  ( Context(..)
  , getContext
  ) where

import "mtl" Control.Monad.Trans
import Data.Maybe
import System.Directory
import System.IO

import Control.Monad.Trans.Uncertain
import Control.Monad.Trans.State.Persistent
import Data.Cache
import qualified Data.HaskellModule as M
import System.Console.Hawk.Context.Dir
import System.Console.Hawk.Context.Paths
import System.Console.Hawk.UserPrelude
import System.Console.Hawk.UserPrelude.Base


data Context = Context
  { contextPaths :: ContextPaths
  , moduleName :: String
  , extensions :: [ExtensionName]
  , modules :: [QualifiedModule]
  } deriving (Eq, Read, Show)

-- | Obtains a Context, either from the cache or from the user prelude.
-- 
-- Must be called inside a `withLock` block, otherwise the cache file
-- might get accessed by two instances of Hawk at once.
getContext :: FilePath -> UncertainT IO Context
getContext contextDir = do
    createDefaultContextDir contextDir
    key <- lift $ getKey preludeFile
    
    -- skip `newContext` if the cached copy is still good.
    withPersistentStateT cacheFile [] $ cached cache key
                                      $ lift
                                      $ newContext paths
  where
    paths = mkContextPaths contextDir
    preludeFile = originalPreludePath paths
    cacheFile   = cachedPreludePath paths
    cache = singletonCache assocCache
    
    getKey f = do
        modifiedTime <- getModificationTime f
        fileSize <- withFile f ReadMode hFileSize
        return (f, modifiedTime, fileSize)

-- | Construct a Context by parsing the user prelude.
newContext :: ContextPaths -> UncertainT IO Context
newContext paths = do
    userPrelude <- readUserPrelude originalFile
    lift $ createDirectoryIfMissing True cacheDir
    compileUserPrelude originalFile canonicalFile userPrelude
    
    return $ Context
           { contextPaths = paths
           , moduleName = fromJust (M.moduleName userPrelude)
           , extensions = M.languageExtensions userPrelude
           , modules = M.importedModules userPrelude
           }
  where
    originalFile  = originalPreludePath paths
    cacheDir      = cacheDirPath paths
    canonicalFile = canonicalPreludePath paths
