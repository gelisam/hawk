{-# LANGUAGE ImplicitParams, OverloadedStrings #-}
module System.Console.Hawk.Config (
      defaultModules
    , recompileConfigIfNeeded
    , getExtensionsFile
    , getModulesFile
    , parseModules
    , recompileConfig
) where

import Control.Applicative ((<$>))
import Control.Monad (when, unless)

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Search as BSS
import Data.Char
import Data.Time
import Data.Maybe (fromJust)
import System.EasyFile
import Text.Printf

import System.Console.Hawk.Config.Cache
import System.Console.Hawk.Config.Compile
import System.Console.Hawk.Config.Parse
import System.Console.Hawk.Lock


defaultModules :: [(String,Maybe String)]
defaultModules = [(representable, Just representable)]
  where
    representable = "System.Console.Hawk.Representable"

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

getOrCreateCacheDir :: IO FilePath
getOrCreateCacheDir = do
    cacheDir <- getCacheDir
    createDirectoryIfMissing True cacheDir
    return cacheDir

-- adjust the prelude to make it loadable from hint.
-- return the module name.
createHintModule :: (?frozenTime :: String) => FilePath -> IO String
createHintModule configFile = do
    source <- adjustSource <$> C8.readFile configFile
    cacheSource source
    return $ forceModuleName source
  where
    adjustSource :: ByteString -> ByteString
    adjustSource = addModuleIfMissing
    
    forceModuleName :: ByteString -> String
    forceModuleName = C8.unpack . fromJust . getModuleName
    
    addModuleIfMissing :: ByteString -> ByteString
    addModuleIfMissing s | getModuleName s == Nothing = addModule configFile s
    addModuleIfMissing s | otherwise                  = s

-- add a module to a string representing a Haskell source file
addModule :: (?frozenTime :: String) => FilePath -> ByteString -> ByteString
addModule configFile source =
    let strippedCode = C8.dropWhile isSpace source
        maybePragma = if "{-#" `C8.isPrefixOf` strippedCode
                        then let (pragma,afterPragma) = BSS.breakAfter "#-}" strippedCode
                             in (Just pragma, afterPragma)
                        else (Nothing,strippedCode)
        line :: Int -> ByteString
        line n = C8.pack $ printf "{-# LINE %d %s #-}" n $ show configFile
        moduleLine = C8.pack $ unwords ["module", defaultModuleName, "where"]
    in case maybePragma of
        (Nothing,c) -> C8.unlines [moduleLine,c]
        (Just pragma,c) -> let n = 1 + C8.length (C8.filter (=='\n') pragma)
                            in C8.unlines [line 1,pragma,moduleLine,line n,c]


-- get the module name from a file if it exists
getModuleName :: ByteString -> Maybe ByteString
getModuleName bs = case BSS.indices (C8.pack "module") bs of
                    [] -> Nothing
                    (i:_) -> Just
                           . C8.takeWhile (\c -> isAlphaNum c || c == '.')
                           . C8.dropWhile isSpace
                           . C8.drop (i + 6) $ bs


-- TODO: error handling
recompileConfig :: IO (String,String)
recompileConfig = do
    clean
    configFile <- getConfigFile
    cacheDir <- getOrCreateCacheDir
    
    extensions <- parseExtensions configFile
    cacheExtensions extensions
    
    modules <- parseModules configFile extensions
    cacheModules modules
    
    currTime <- (filter isDigit . show <$> getCurrentTime)
    let ?frozenTime = currTime in do
      moduleName <- createHintModule configFile
      
      sourceFile <- getSourceFile
      compiledFile <- getCompiledFile
      compile sourceFile compiledFile cacheDir
      
      lastModTime <- getModificationTime configFile
      configInfosFile <- getConfigInfosFile
      writeFile configInfosFile $ unlines [sourceFile
                                           ,moduleName
                                           ,show lastModTime]
      
      return (sourceFile, moduleName)
  where
    clean :: IO ()
    clean = do
        dir <- getCacheDir
        dirExists <- doesDirectoryExist dir
        when (dirExists) (removeDirectoryRecursive dir)
