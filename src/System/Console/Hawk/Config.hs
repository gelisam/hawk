{-# LANGUAGE OverloadedStrings #-}
module System.Console.Hawk.Config (
      defaultModules
    , defaultPrelude
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
import Data.Monoid ((<>))
import System.EasyFile
import Text.Printf

import System.Console.Hawk.Config.Cache
import System.Console.Hawk.Config.Compile
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

-- adjust the prelude to make it loadable from hint.
-- return the generated source.
parseHintModule :: FilePath
                -> FilePath
                -> [ExtensionName]
                -> [QualifiedModule]
                -> IO ByteString
parseHintModule sourceFile configFile extensions modules = do
    adjustSource <$> C8.readFile configFile
  where
    adjustSource :: ByteString -> ByteString
    adjustSource = addPreludeIfMissing . addModuleIfMissing

    addModuleIfMissing :: ByteString -> ByteString
    addModuleIfMissing s | getModuleName s == Nothing = addModule configFile s
    addModuleIfMissing s | otherwise                  = s
    
    addPreludeIfMissing :: ByteString -> ByteString
    addPreludeIfMissing s | "NoImplicitPrelude" `elem` extensions = s
    addPreludeIfMissing s | "Prelude" `elem` map fst modules      = s
    addPreludeIfMissing s | otherwise = addImport "Prelude" configFile s

-- add a module to a string representing a Haskell source file
addModule :: FilePath -> ByteString -> ByteString
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

-- add an import statement to a string representing a Haskell source file
addImport :: String -> FilePath -> ByteString -> ByteString
addImport moduleName configFile source =
    let (premodule,postmodule)   = BSS.breakAfter "module " source
        (prewhere,postwhere)     = BSS.breakAfter " where" postmodule
        (prenewline,postnewline) = BSS.breakAfter "\n" postwhere
        preimports = premodule <> prewhere <> prenewline
        postimports = postnewline
        line :: Int -> ByteString
        line n = C8.pack $ printf "{-# LINE %d %s #-}" n $ show configFile
        importLine = C8.pack $ unwords ["import", moduleName]
        n = 1 + C8.length (C8.filter (=='\n') preimports)
        extraLines = C8.unlines [importLine, line n]
    in preimports <> extraLines <> postimports


-- get the module name from a file if it exists
getModuleName :: ByteString -> Maybe ByteString
getModuleName bs = case BSS.indices (C8.pack "module") bs of
                    [] -> Nothing
                    (i:_) -> Just
                           . C8.takeWhile (\c -> isAlphaNum c || c == '.')
                           . C8.dropWhile isSpace
                           . C8.drop (i + 6) $ bs

-- same, but crash if there is no module
forceModuleName :: ByteString -> String
forceModuleName = C8.unpack . fromJust . getModuleName


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
    
    source <- parseHintModule sourceFile configFile extensions modules
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
