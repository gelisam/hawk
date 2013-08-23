{-# LANGUAGE OverloadedStrings #-}
module System.Console.Hawk.Config (
      defaultModules
    , getConfigFileAndModuleName
    , getModulesFile
    , recompileConfig
) where

import Control.Applicative ((<$>))
import Control.Exception (bracket)
import Control.Monad (when, unless)

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Search as BSS
import Data.Char
import Data.List
import Data.Maybe (catMaybes)
import Data.Time
import Language.Haskell.Exts
import System.EasyFile
import System.Exit
import System.IO
import System.Process

import System.Console.Hawk.Lock


defaultModules :: [(String,Maybe String)]
defaultModules = flip zip (repeat Nothing) 
                       [ "System.Console.Hawk.Representable"
                       , "GHC.Num"
                       , "GHC.Real"
                       , "GHC.Types"
                       , "Data.ByteString.Lazy.Char8"
                       , "Data.Bool"
                       , "Data.Char"
                       , "Data.Either"
                       , "Data.Eq"
                       , "Data.Function"
                       , "Data.Int"
                       , "Data.Maybe"
                       , "Data.Ord"]

getConfigDir :: IO FilePath
getConfigDir = (</> ".hawk" ) <$> getHomeDirectory

getConfigFile :: IO FilePath
getConfigFile = (</> "prelude.hs") <$> getConfigDir

getCacheDir :: IO FilePath
getCacheDir = (</> "cache") <$> getConfigDir

getConfigInfosFile :: IO FilePath
getConfigInfosFile = (</> "configInfos") <$> getCacheDir

getModulesFile :: IO FilePath
getModulesFile = (</> "modules") <$> getCacheDir


-- --
-- From now the code is heavy, it needs a refactoring (renaming)

-- maybe (file name, module name)
-- TODO: error handling

getConfigFileAndModuleName :: IO (Maybe (String,String)) -- ^ Maybe (FileName,ModuleName)
getConfigFileAndModuleName = do
    withLock $ do
        dir <- getConfigDir
        dirExists <- doesDirectoryExist dir
        if (not dirExists)
         then createDirectoryIfMissing True dir >> return Nothing
         else do
            configFile <- getConfigFile
            configFileExists <- doesFileExist configFile
            unless configFileExists $ do
              writeFile configFile "import qualified Prelude as P\n"
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
                         else return $ Just (fileName,moduleName)
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
                        then let (pragma,afterPragma) = BSS.breakOn "#-}" strippedCode
                             in (Just pragma,C8.drop 3 afterPragma)
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
    modules <- parseFileAndGetModules []
    --print modules
    C8.writeFile modulesFile (C8.pack $ show modules)
    where parseFileAndGetModules exts = do
            result <- parseFileWithExts exts sourceFile
            case result of
                ParseOk (Module _ _ _ _ _ importDeclarations _) -> do
                    return $ concatMap toHintModules importDeclarations
                ParseFailed srcLog err -> do
                    if " is not enabled" `isSuffixOf` err
                        -- if parsing failes because of some extension missing
                        -- then add that extension and retry
                        then parseFileAndGetModules ((read . head $ words err):exts)
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
              otherwise -> undefined


-- TODO: error handling
recompileConfig :: IO (Maybe (String,String))
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
    return $ Just (configFileWithModule,C8.unpack moduleName)
    where
        clean :: IO ()
        clean = do
            dir <- getCacheDir
            dirExists <- doesDirectoryExist dir
            when (dirExists) (removeDirectoryRecursive dir)
