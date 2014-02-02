{-# LANGUAGE PackageImports #-}
-- | Everything we need to know in order to evaluate a user expression,
--   except for the user expression itself.
module System.Console.Hawk.Context.Base
  ( EvalContext(..)
  , getEvalContext
  ) where

import Control.Monad
import "mtl" Control.Monad.Trans
import System.Directory
import System.EasyFile hiding (getCurrentDirectory,getModificationTime)
import System.IO

import Control.Monad.Trans.Uncertain
import Control.Monad.Trans.State.Persistent
import Data.Cache
import System.Console.Hawk.UserPrelude
import System.Console.Hawk.UserPrelude.Base
import System.Console.Hawk.UserPrelude.Cache
import System.Console.Hawk.UserPrelude.Parse


data EvalContext = EvalContext
  { originalPreludePath :: FilePath
  , canonicalPrelude :: FilePath
  , compiledPrelude :: FilePath
  , moduleName :: String
  , extensions :: [ExtensionName]
  , modules :: [QualifiedModule]
  } deriving (Eq, Read, Show)

-- | Obtains an EvalContext, either from the cache or from the user prelude.
getEvalContext :: FilePath -> Bool -> IO EvalContext
getEvalContext confDir True = newEvalContext confDir
getEvalContext confDir False = do
  -- skip `newEvalContext` if the cached copy is still good.
  let preludeFile = getConfigFile confDir
  let cacheFile   = getEvalContextFile confDir
  key <- getKey preludeFile
  let cache = singletonCache assocCache
  withPersistentStateT cacheFile [] $ cached cache key
                                    $ lift $ newEvalContext confDir
  where
    getKey f = do
        modifiedTime <- getModificationTime f
        fileSize <- withFile f ReadMode hFileSize
        return (f, modifiedTime, fileSize)

-- | Construct an EvalContext by parsing the user prelude.
newEvalContext :: FilePath -> IO EvalContext
newEvalContext confDir = do
    let originalPreludePath' = getConfigFile confDir
    
    (canonicalPrelude', moduleName') <- recompileConfig confDir
    extensions' <- readExtensions originalPreludePath'
    modules' <- readModules extensions' originalPreludePath'
    
    -- I think it hint will automatically use the version we have just
    -- compiled if we give it the path to the .hs file.
    -- 
    -- TODO: check whether using .o or .hi instead works
    -- and whether it makes any difference.
    let compiledPrelude' = canonicalPrelude'
    
    return $ EvalContext
           { originalPreludePath = originalPreludePath'
           , canonicalPrelude = canonicalPrelude'
           , compiledPrelude = compiledPrelude'
           , moduleName = moduleName'
           , extensions = extensions'
           , modules = modules'
           }
