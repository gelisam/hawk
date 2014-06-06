{-# LANGUAGE PackageImports #-}
-- | About the directory in which the context is persited.
module System.Console.Hawk.Context.Dir
  ( createDefaultContextDir
  , findContextFromCurrDirOrDefault
  , checkContextDir
  ) where

import Control.Monad
import "mtl" Control.Monad.Trans
import System.Directory
import System.EasyFile (readable)
import System.FilePath

import Control.Monad.Trans.Uncertain
import System.Console.Hawk.Context.Paths
import System.Console.Hawk.UserPrelude.Defaults
import System.Directory.Extra


-- | Create a default context
createDefaultContextDir :: ContextPaths -> UncertainT IO ()
createDefaultContextDir paths = do
    _ <- checkContextDir contextDir
    liftIO $ do
      createDirectoryIfMissing True contextDir
      preludeExists <- doesFileExist preludeFile
      unless preludeExists $ writeFile preludeFile defaultPrelude
  where
    contextDir = contextDirPath paths
    preludeFile = originalPreludePath paths

-- | Find a project context
findContext :: FilePath -> IO (Maybe FilePath)
findContext startDir =
    foldM (maybe validDirOrNothing (const . return . Just)) Nothing possibleContextDirs
  where
    mkHawkPath = (</> ".hawk")
    possibleContextDirs = map mkHawkPath (ancestors startDir)
    
    validDirOrNothing dir = do
      dirExists <- doesDirectoryExist dir
      if dirExists
       then do
         permissions <- getPermissions dir
         if writable permissions && searchable permissions
           then do
             prelude <- findFile [dir] "prelude.hs"
             case prelude of
               Nothing -> return Nothing
               Just f -> do
                 preludePermissions <- getPermissions f
                 if System.EasyFile.readable preludePermissions
                   then return $ Just dir
                   else return Nothing
           else return Nothing
       else return Nothing

-- | Find a project context starting from the current working directory
findContextFromCurrDir :: IO (Maybe FilePath)
findContextFromCurrDir = getCurrentDirectory >>= findContext

-- | Find a project context or return the default
findContextFromCurrDirOrDefault :: IO FilePath
findContextFromCurrDirOrDefault = do
    maybeProjectContextDir <- findContextFromCurrDir
    case maybeProjectContextDir of
      Nothing -> getDefaultContextDir
      Just projectContextDir -> return projectContextDir

-- | Check if a directory is a valid context and return true if the directory
-- doesn't exist and the parent has the right permissions
checkContextDir :: FilePath -> UncertainT IO Bool
checkContextDir dir = do
    fileExists <- liftIO $ doesFileExist dir
    when fileExists $ fail $ concat [
       "context directory '",dir,"' cannot be"
      ,"created because a file with the same"
      ,"name exists"]
    dirExists <- liftIO $ doesDirectoryExist dir
    if dirExists
      then do
        permissions <- liftIO $ getPermissions dir
        when (not $ writable permissions) $ fail $ concat [
           "cannot use '",dir,"' as context directory because it is not "
          ,"writable"]
        when (not $ searchable permissions) $ fail $ concat [
           "cannot use '",dir,"' as context directory because it is not "
          ,"searchable"]
        return False
      else do
        -- if the directory doesn't exist then its parent must be writable
        -- and searchable
        let parent = case takeDirectory dir of {"" -> ".";p -> p}
        permissions <- liftIO $ getPermissions parent
        when (not $ writable permissions) $ fail $ concat[
           "cannot create context directory '",dir,"' because the parent "
          ," directory is not writable (",show permissions,")"]
        when (not $ searchable permissions) $ fail $ concat[
           "cannot create context directory '",dir,"' because the parent "
          ," directory is not searchable (",show permissions,")"]
        warn $ concat ["directory '",dir,"' doesn't exist, creating a "
                      ,"default one"]
        return True

getDefaultContextDir :: IO FilePath
getDefaultContextDir = do
    home <- getHomeDirectory
    return $ home </> ".hawk"
