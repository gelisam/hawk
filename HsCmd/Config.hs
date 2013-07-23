module HsCmd.Config where

import Control.Applicative ((<$>))
import Control.Exception (bracket)
import Control.Monad (when)

import Data.Char
import Data.Time
import System.EasyFile
import System.Exit
import System.IO
import System.Process


defaultModules :: [(String,Maybe String)]
defaultModules = flip zip (repeat Nothing) 
                       [ "HsCmd.Representable"
                       , "GHC.Types"
                       , "Data.Bool"
                       , "Data.Char"
                       , "Data.Either"
                       , "Data.Eq"
                       , "Data.Function"
                       , "Data.Int"
                       , "Data.Ord"]

getConfigDir :: IO FilePath
getConfigDir = (</> ".hs" ) <$> getHomeDirectory

getDefaultConfigFile :: IO FilePath
getDefaultConfigFile = (</> "modules" ) <$> getConfigDir

getToolkitFile :: IO FilePath
getToolkitFile = (</> "toolkit.hs") <$> getConfigDir

getCacheDir :: IO FilePath
getCacheDir = (</> "cache") <$> getConfigDir

getToolkitInfosFile :: IO FilePath
getToolkitInfosFile = (</> "toolkitInfos") <$> getCacheDir


-- --
-- From now the code is heavy, it needs a refactoring (renaming)

-- maybe (file name, module name)
-- TODO: error handling
getToolkitFileAndModuleName :: IO (Maybe (String,String))
getToolkitFileAndModuleName = do
    dir <- getConfigDir
    dirExists <- doesDirectoryExist dir
    if (not dirExists)
     then createDirectoryIfMissing True dir >> return Nothing
     else do
        toolkitFile <- getToolkitFile
        toolkitFileExists <- doesFileExist toolkitFile
        if toolkitFileExists
         then do
            toolkitInfosFile <- getToolkitInfosFile
            toolkitInfosExists <- doesFileExist toolkitInfosFile
            if toolkitInfosExists
              then do
                  toolkitInfos <- lines <$> readFile toolkitInfosFile
                  if length toolkitInfos /= 3 -- error
                    then recompile
                    else do
                        let [fileName,moduleName,rawLastModTime] = toolkitInfos
                        let lastModTime = (read rawLastModTime :: UTCTime)
                        currModTime <- getModificationTime toolkitFile
                        if currModTime > lastModTime
                         then recompile
                         else return $ Just (fileName,moduleName)
              else recompile
         else return Nothing

-- TODO: error handling
recompile :: IO (Maybe (String,String))
recompile = do
    clean
    toolkitFile <- getToolkitFile
    currTime <- (filter isDigit . show <$> getCurrentTime)
    let moduleName = "HsCmd.M" ++ currTime
    configDir <- getCacheDir
    createDirectoryIfMissing True configDir
    let compiledFile = configDir </> ("toolkit" ++ currTime)
    let toolkitFileWithModule = compiledFile ++ ".hs"
    toolkitCode <- addModule moduleName <$> readFile toolkitFile
    writeFile toolkitFileWithModule toolkitCode
    let err = compiledFile ++ ".ghc.err"
    compExitCode <- bracket (openFile err WriteMode) hClose $ \h ->
            waitForProcess =<< runProcess "ghc" ["--make"
                                               , toolkitFileWithModule
                                               , "-i"
                                               , "-ilib"
                                               , "-fforce-recomp"
                                               , "-v0"
                                               , "-o",compiledFile]
                                          (Just configDir)
                                          Nothing
                                          Nothing
                                          Nothing
                                          (Just h)
    
    when (compExitCode /= ExitSuccess) $ do
        ghcErr <- readFile err
        let msg = unlines $ ["Error detected while loading "
                          ++ "configuration file: " ++ toolkitFile]
                          ++ lines (if null ghcErr
                                      then show compExitCode
                                      else ghcErr)
                          ++ ["","Please check the file for errors."]
        putStrLn msg
        exitFailure
    lastModTime <- getModificationTime toolkitFile
    toolkitInfosFile <- getToolkitInfosFile
    writeFile toolkitInfosFile $ unlines [toolkitFileWithModule
                                         ,moduleName
                                         ,show lastModTime]
    return $ Just (toolkitFileWithModule,moduleName)
    where
        addModule :: String -> String -> String
        addModule moduleName code = 
            let strippedCode = dropWhile isSpace code
                moduleLine = "module " ++ moduleName ++ " where"
            in unlines (if head strippedCode == '{'
                         then
                           let afterPragma = dropWhile (/= '}') strippedCode
                           in [takeWhile (/= '}') strippedCode ++
                               take 1 afterPragma
                              , moduleLine
                              , drop 1 afterPragma]
                         else [moduleLine,strippedCode])

        clean :: IO ()
        clean = do
            dir <- getCacheDir
            dirExists <- doesDirectoryExist dir
            when (dirExists) (removeDirectoryRecursive dir)
