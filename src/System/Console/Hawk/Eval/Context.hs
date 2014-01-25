{-# LANGUAGE PackageImports #-}
-- | Everything we need to know in order to evaluate a user expression,
--   except for the user expression itself.
module System.Console.Hawk.Eval.Context
  ( EvalContext(..)
  , getEvalContext
  ) where

import "mtl" Control.Monad.Trans
import System.Directory
import System.IO

import Control.Monad.Trans.State.Persistent
import Data.Cache
import System.Console.Hawk.Args.Spec
import System.Console.Hawk.Config
import System.Console.Hawk.Config.Base
import System.Console.Hawk.Config.Cache
import System.Console.Hawk.Config.Parse


data EvalContext = EvalContext
  { originalPreludePath :: FilePath
  , canonicalPrelude :: FilePath
  , compiledPrelude :: FilePath
  , moduleName :: String
  , extensions :: [ExtensionName]
  , modules :: [QualifiedModule]
  } deriving (Eq, Read, Show)

getEvalContext :: PreludeSpec -> IO EvalContext
getEvalContext UseUserPrelude = newEvalContext
getEvalContext DetectPrelude = do
    -- skip `newEvalContext` if the cached copy is still good.
    preludeFile <- getConfigFile
    cacheFile <- getEvalContextFile
    key <- getKey preludeFile
    let cache = singletonCache assocCache
    withPersistentStateT cacheFile [] $ cached cache key $ lift newEvalContext
  where
    getKey f = do
        modifiedTime <- getModificationTime f
        fileSize <- withFile f ReadMode hFileSize
        return (f, modifiedTime, fileSize)

newEvalContext :: IO EvalContext
newEvalContext = do
    -- currently, only ~/.hawk/prelude.hs is supported.
    originalPreludePath' <- getConfigFile
    
    (canonicalPrelude', moduleName') <- recompileConfig
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
