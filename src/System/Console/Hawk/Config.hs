{-# LANGUAGE OverloadedStrings #-}
module System.Console.Hawk.Config where

import Control.Applicative ((<$>))
import Control.Exception (bracket)
import Control.Monad (when, unless)

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Search as BSS
import Data.Char
import Data.List
import Data.Time
import Language.Haskell.Exts
import System.EasyFile
import System.Exit
import System.IO
import System.Process


defaultModules :: [(String,Maybe String)]
defaultModules = [("System.Console.Hawk.Representable",
                   Just "System.Console.Hawk.Representable")]

(<//>) :: IO FilePath -- the left filepath in the IO monad
       -> FilePath -- the right filepath
       -> IO FilePath -- the filepath resulting
lpath <//> rpath = (</> rpath) <$> lpath

getConfigDir :: IO FilePath
getConfigDir = getHomeDirectory <//> ".hawk"

getConfigFile :: IO FilePath
getConfigFile = getConfigDir <//> "prelude.hs"

getCacheDir :: IO FilePath
getCacheDir = getConfigDir <//> "cache"

getConfigInfosFile :: IO FilePath
getConfigInfosFile = getCacheDir <//> "configInfos"

getModulesFile :: IO FilePath
getModulesFile = getCacheDir <//> "modules"


-- --
-- From now the code is heavy, it needs a refactoring (renaming)

-- maybe (file name, module name)
-- TODO: error handling

recompileConfigIfNeeded :: IO (String,String) -- ^ Maybe (FileName,ModuleName)
recompileConfigIfNeeded = do
    dir <- getConfigDir
    dirExists <- doesDirectoryExist dir
    unless dirExists $
        createDirectoryIfMissing True dir
    configFile <- getConfigFile
    configFileExists <- doesFileExist configFile
    unless configFileExists $
        writeFile configFile $
            unlines
            [ "import Prelude"
            , "import qualified Data.ByteString.Lazy.Char8 as B"
            , "import qualified Data.List as L"]
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

-- retrieve the real configuration file, that is the configuration
-- file with a module name. If the configuration file doesn't have a module
-- name than create one and set it in a cache file
getConfigFileWithModule :: FilePath -- ^ the user config file
                        -> ByteString -- ^ module name in case the config file
                                      --   doesn't have it
                        -> FilePath -- ^ output file, used to put the config file
                                    --   with the random module
                        -> IO (FilePath,ByteString)
getConfigFileWithModule configFile moduleName configFileWithModule = do
    configCode <- C8.readFile configFile
    case getModuleName configCode of
        Just moduleName' -> return (configFile,moduleName')
        Nothing -> let configCodeWithModule = addModule moduleName
                                                        configCode
                   in do
                      C8.writeFile configFileWithModule configCodeWithModule
                      return (configFileWithModule,moduleName)

-- add a module to a string representing a Haskell source file
addModule :: ByteString -- ^ module name
          -> ByteString -- ^ haskell code
          -> ByteString -- ^ result
addModule moduleName code =
    let strippedCode = C8.dropWhile isSpace code
        maybePragma = if "{-#" `C8.isPrefixOf` strippedCode
                        then let (pragma,afterPragma) = BSS.breakAfter "#-}" strippedCode
                             in (Just pragma, afterPragma)
                        else (Nothing,strippedCode)
        moduleLine = C8.unwords [C8.pack "module", moduleName, C8.pack "where"]
    in case maybePragma of
        (Nothing,c) -> C8.unlines [moduleLine,c]
        (Just pragma,c) -> C8.unlines [pragma,moduleLine,c]


-- get the module name from a file if it exists
getModuleName :: ByteString -> Maybe ByteString
getModuleName bs = case BSS.indices (C8.pack "module") bs of
                    [] -> Nothing
                    (i:_) -> Just
                           . C8.takeWhile (\c -> isAlphaNum c || c == '.')
                           . C8.dropWhile isSpace
                           . C8.drop (i + 6) $ bs

-- compile a haskell file
-- TODO: this should return the error instead of print it and exit
compile :: FilePath -- ^ the source file
        -> FilePath -- ^ the output file
        -> FilePath -- ^ the directory used for compiler files
        -> FilePath -- ^ error file
        -> IO ()
compile sourceFile outputFile dir err = do
    compExitCode <- bracket (openFile err WriteMode) hClose $ \h ->
            waitForProcess =<< runProcess "ghc" ["--make"
                                               , sourceFile
                                               , "-i"
                                               , "-ilib"
                                               , "-fforce-recomp"
                                               , "-v0"
                                               , "-o",outputFile]
                                          (Just dir)
                                          Nothing
                                          Nothing
                                          Nothing
                                          (Just h)
    when (compExitCode /= ExitSuccess) $ do
        ghcErr <- readFile err
        let msg = unlines $ ["Error detected while loading "
                          ++ "configuration file: " ++ sourceFile]
                          ++ lines (if null ghcErr
                                      then show compExitCode
                                      else ghcErr)
                          ++ ["","Please check the file for errors."]
        putStrLn msg -- TODO: use stderr
        exitFailure

-- create the module file by extracting modules from the given file
createModulesFile :: FilePath -- ^ the source file from which extract modules
                  -> IO ()
createModulesFile sourceFile = do
    modulesFile <- getModulesFile
    modules <- parseFileAndGetModules sourceFile []
    --print modules
    C8.writeFile modulesFile (C8.pack $ show modules)

parseFileAndGetModules :: FilePath
                       -> [Extension]
                       -> IO [(String,Maybe String)]
parseFileAndGetModules sourceFile = go
    where go exts = do
            result <- parseFileWithExts exts sourceFile
            case result of
                ParseOk (Module _ _ _ _ _ importDeclarations _) -> do
                    return $ concatMap toHintModules importDeclarations
                ParseFailed srcLog err -> do
                    if " is not enabled" `isSuffixOf` err
                        -- if parsing failes because of some extension missing
                        -- then add that extension and retry
                        then go ((read . head $ words err):exts)
                        else do
                            putStrLn $ concat ["Error parsing file "
                                             , sourceFile,"\n"
                                             , show srcLog,": ",err]
                            exitFailure
          toHintModules :: ImportDecl -> [(String,Maybe String)]
          toHintModules importDecl =
            case importDecl of
              ImportDecl _ (ModuleName mn) False _ _ Nothing _ -> [(mn,Nothing)]
              ImportDecl _ (ModuleName mn) False _ _ (Just (ModuleName s)) _ -> 
                                    [(mn,Nothing),(mn,Just s)]
              ImportDecl _ (ModuleName mn) True _ _ Nothing _ -> [(mn,Just mn)]
              ImportDecl _ (ModuleName mn) True _ _ (Just (ModuleName s)) _ ->
                                    [(mn,Just s)]
              _ -> undefined


-- TODO: error handling
recompileConfig :: IO (String,String)
recompileConfig = do
    clean
    configFile <- getConfigFile
    currTime <- (filter isDigit . show <$> getCurrentTime)
    cacheDir <- getOrCreateCacheDir
    let compiledFile = cacheDir </> ("config" ++ currTime)
    (configFileWithModule,moduleName) <- getConfigFileWithModule 
                                               configFile
                                               (C8.pack $ "Hawk.M" ++ currTime)
                                               (compiledFile ++ ".hs")
    let err = compiledFile ++ ".ghc.err"
    compile configFileWithModule compiledFile cacheDir err
    lastModTime <- getModificationTime configFile
    configInfosFile <- getConfigInfosFile
    writeFile configInfosFile $ unlines [configFileWithModule
                                         ,C8.unpack moduleName
                                         ,show lastModTime]
    createModulesFile configFile
    return (configFileWithModule,C8.unpack moduleName)
    where
        clean :: IO ()
        clean = do
            dir <- getCacheDir
            dirExists <- doesDirectoryExist dir
            when (dirExists) (removeDirectoryRecursive dir)
