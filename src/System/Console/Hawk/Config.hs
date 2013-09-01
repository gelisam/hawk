{-# LANGUAGE OverloadedStrings #-}
module System.Console.Hawk.Config (
      defaultModules
    , defaultPrelude
    , recompileConfigIfNeeded
    , getExtensionsFile
    , getModulesFile
    , parseModules
    , recompileConfig
    , recompileConfig'
) where

import Control.Applicative ((<$>))
import Control.Monad (when, unless)

import Data.Time
import System.EasyFile

import System.Console.Hawk.Config.Base
import System.Console.Hawk.Config.Cache
import System.Console.Hawk.Config.Compile
import System.Console.Hawk.Config.Extend
import System.Console.Hawk.Config.Parse
import System.Console.Hawk.Lock


defaultModules :: [QualifiedModule]
defaultModules = map fully_qualified [ "Prelude"
                                     , "System.Console.Hawk.Representable"
                                     , "System.IO.Unsafe"
                                     ]
  where
    fully_qualified x = (x, Just x)

defaultPrelude :: String
defaultPrelude = unlines
               [ "{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}"
               , "import Prelude"
               , "import qualified Data.ByteString.Lazy.Char8 as B"
               , "import qualified Data.List as L"
               ]

-- --
-- From now the code is heavy, it needs a refactoring (renaming)

-- maybe (file name, module name)
-- TODO: error handling

recompileConfigIfNeeded :: IO (String,String) -- ^ Maybe (FileName,ModuleName)
recompileConfigIfNeeded = withLock $ do
    dir <- getConfigDir
    dirExists <- doesDirectoryExist dir
    unless dirExists $
        createDirectoryIfMissing True dir
    configFile <- getConfigFile
    configFileExists <- doesFileExist configFile
    unless configFileExists $
        writeFile configFile defaultPrelude
    configInfosFile <- getConfigInfosFile
    configInfosExists <- doesFileExist configInfosFile
    if configInfosExists
      then do
          configInfos <- lines <$> readFile configInfosFile
          if length configInfos /= 3 -- error
            then recompileConfig
            else do
                let [fileName,moduleName,rawLastModTime] = configInfos
                let withoutExt = dropExtension fileName
                let hiFile = withoutExt ++ ".hi"
                hiFileDoesntExist <- not <$> doesFileExist hiFile
                let objFile = withoutExt ++ ".o"
                objFileDoesntExist <- not <$> doesFileExist objFile
                let lastModTime = (read rawLastModTime :: UTCTime)
                currModTime <- getModificationTime configFile
                if hiFileDoesntExist || objFileDoesntExist 
                                     || currModTime > lastModTime
                 then recompileConfig
                 else return (fileName,moduleName)
      else recompileConfig


-- TODO: error handling
recompileConfig :: IO (String,String)
recompileConfig = do
  configFile <- getConfigFile
  cacheDir <- getCacheDir
  sourceFile <- getSourceFile
  extensionFile <- getExtensionsFile
  modulesFile <- getModulesFile
  compiledFile <- getCompiledFile
  configInfosFile <- getConfigInfosFile
  recompileConfig' configFile
                   cacheDir
                   sourceFile
                   extensionFile
                   modulesFile
                   compiledFile
                   configInfosFile

recompileConfig' :: FilePath -- ^ config file
                 -> FilePath -- ^ cache dir
                 -> FilePath -- ^ source file
                 -> FilePath -- ^ output extensions cache file
                 -> FilePath -- ^ output modules cache file
                 -> FilePath -- ^ output compiled file
                 -> FilePath -- ^ output config info path
                 -> IO (String,String)
recompileConfig' configFile
                 cacheDir
                 sourceFile
                 extensionsFile
                 modulesFile
                 compiledFile
                 configInfosFile = do
    clean
    createDirectoryIfMissing True cacheDir
    
    extensions <- parseExtensions configFile
    cacheExtensions extensionsFile extensions
    
    modules <- parseModules configFile extensions
    cacheModules modulesFile modules
    
    source <- extendSource configFile extensions modules <$> parseSource configFile
    cacheSource sourceFile source
    
    compile sourceFile compiledFile cacheDir
    
    let moduleName = forceModuleName source
    lastModTime <- getModificationTime configFile
    writeFile configInfosFile $ unlines [sourceFile
                                         ,moduleName
                                         ,show lastModTime]
    
    return (sourceFile, moduleName)
  where
    clean :: IO ()
    clean = do
        dirExists <- doesDirectoryExist cacheDir
        when (dirExists) (removeDirectoryRecursive cacheDir)
