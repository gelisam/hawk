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
import System.Console.Hawk.UserPrelude
import System.Console.Hawk.UserPrelude.Base
import System.Console.Hawk.UserPrelude.Cache


data Context = Context
  { originalPreludePath :: FilePath
  , canonicalPrelude :: FilePath
  , compiledPrelude :: FilePath
  , moduleName :: String
  , extensions :: [ExtensionName]
  , modules :: [QualifiedModule]
  } deriving (Eq, Read, Show)

-- | Obtains a Context, either from the cache or from the user prelude.
-- 
-- Must be called inside a `withLock` block, otherwise the cache file
-- might get accessed by two instances of Hawk at once.
getContext :: FilePath -> UncertainT IO Context
getContext confDir = do
    createDefaultContextDir confDir
    -- skip `newContext` if the cached copy is still good.
    let preludeFile = getUserPreludeFile confDir
    let cacheFile   = getContextFile confDir
    key <- lift $ getKey preludeFile
    let cache = singletonCache assocCache
    withPersistentStateT cacheFile [] $ cached cache key
                                      $ lift
                                      $ newContext confDir
  where
    getKey f = do
        modifiedTime <- getModificationTime f
        fileSize <- withFile f ReadMode hFileSize
        return (f, modifiedTime, fileSize)

-- | Construct a Context by parsing the user prelude.
newContext :: FilePath -> UncertainT IO Context
newContext confDir = do
    let originalPreludePath' = getUserPreludeFile confDir
    let canonicalPreludePath' = getSourceFile confDir
    userPrelude <- readUserPrelude originalPreludePath'
    compileUserPrelude originalPreludePath' canonicalPreludePath' userPrelude
    -- extensions' <- readExtensions userPrelude'
    -- modules' <- readModules extensions' userPrelude'
    
    -- I think it hint will automatically use the version we have just
    -- compiled if we give it the path to the .hs file.
    -- 
    -- TODO: check whether using .o or .hi instead works
    -- and whether it makes any difference.
    let compiledPreludePath' = canonicalPreludePath'
    
    return $ Context
           { originalPreludePath = originalPreludePath'
           , canonicalPrelude = canonicalPreludePath'
           , compiledPrelude = compiledPreludePath'
           , moduleName = fromJust (M.moduleName userPrelude)
           , extensions = M.languageExtensions userPrelude
           , modules = M.importedModules userPrelude
           }
