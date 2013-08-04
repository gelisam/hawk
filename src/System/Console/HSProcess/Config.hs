{- 
  Copyright 2013 Mario Pastorelli (pastorelli.mario@gmail.com)
 
    This file is part of HSProcess.
 
  HSProcess is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.
 
  HSProcess is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with HSProcess.  If not, see <http://www.gnu.org/licenses/>.
-}
module System.Console.HSProcess.Config where

import Control.Applicative ((<$>))
import Control.Exception (bracket)
import Control.Monad (when)

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Search as BSS
import Data.Char
import Data.Time
import System.EasyFile
import System.Exit
import System.IO
import System.Process


defaultModules :: [(String,Maybe String)]
defaultModules = flip zip (repeat Nothing) 
                       [ "System.Console.HSProcess.Representable"
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
getConfigDir = (</> ".hsp" ) <$> getHomeDirectory

getDefaultConfigFile :: IO (Maybe FilePath)
getDefaultConfigFile = do
    modFile <- (</> "modules" ) <$> getConfigDir
    modFileExists <- doesFileExist modFile
    return (if modFileExists then Just modFile else Nothing)

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

getToolkitFileAndModuleName :: IO (Maybe (String,String)) -- ^ Maybe (FileName,ModuleName)
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
                        let withoutExt = dropExtension fileName
                        let hiFile = withoutExt ++ ".hi"
                        hiFileDoesntExist <- not <$> doesFileExist hiFile
                        let objFile = withoutExt ++ ".o"
                        objFileDoesntExist <- not <$> doesFileExist objFile
                        let lastModTime = (read rawLastModTime :: UTCTime)
                        currModTime <- getModificationTime toolkitFile
                        if hiFileDoesntExist || objFileDoesntExist 
                                             || currModTime > lastModTime
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
    configDir <- getCacheDir
    createDirectoryIfMissing True configDir
    let compiledFile = configDir </> ("toolkit" ++ currTime)
--    toolkitCode <- addOrGetModule moduleName <$> C8.readFile toolkitFile
    toolkitCode <- C8.readFile toolkitFile
    (toolkitFileWithModule,moduleName) <- 
            case getModuleName toolkitCode of
                Just moduleName' -> return (toolkitFile,moduleName')
                Nothing -> 
                    let randModuleName = C8.pack $ "HSProcess.M" ++ currTime
                        toolkitFileWithModule = compiledFile ++ ".hs"
                    in do let toolkitCodeWithModule = addModule randModuleName toolkitCode
                          C8.writeFile toolkitFileWithModule toolkitCodeWithModule
                          return (toolkitFileWithModule,randModuleName)
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
                                         ,C8.unpack moduleName
                                         ,show lastModTime]
    return $ Just (toolkitFileWithModule,C8.unpack moduleName)
    where
        addModule :: ByteString -> ByteString -> ByteString
        addModule moduleName code = 
            let strippedCode = C8.dropWhile isSpace code
                moduleLine = C8.unwords [C8.pack "module", moduleName, C8.pack "where"]
            in C8.unlines (if C8.head strippedCode == '{'
                         then
                           let afterPragma = C8.dropWhile (/= '}') strippedCode
                           in [C8.append (C8.takeWhile (/= '}') strippedCode)
                                         (C8.take 1 afterPragma)
                              , moduleLine
                              , C8.drop 1 afterPragma]
                         else [moduleLine,strippedCode])
        getModuleName :: ByteString -> Maybe ByteString
        getModuleName bs = case BSS.indices (C8.pack "module") bs of
                            [] -> Nothing
                            (i:_) -> Just
                                   . C8.takeWhile (\c -> isAlphaNum c || c == '.')
                                   . C8.dropWhile isSpace
                                   . C8.drop (i + 6) $ bs
        clean :: IO ()
        clean = do
            dir <- getCacheDir
            dirExists <- doesDirectoryExist dir
            when (dirExists) (removeDirectoryRecursive dir)
