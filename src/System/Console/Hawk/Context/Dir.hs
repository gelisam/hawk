{-# LANGUAGE PackageImports #-}
-- | Everything we need to know in order to evaluate a user expression,
--   except for the user expression itself.
module System.Console.Hawk.Context.Dir
  ( createDefaultContextDir
  , findContextFromCurrDirOrDefault
  , checkContextDir
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


-- | Create a default context
createDefaultContextDir :: FilePath -> IO ()
createDefaultContextDir dir = do
  createDirectoryIfMissing True dir
  let preludeFile = getConfigFile dir
  preludeExists <- doesFileExist preludeFile
  unless preludeExists $ writeFile preludeFile defaultPrelude

-- | Find a project context
findContext :: FilePath -> IO (Maybe FilePath)
findContext startDir = do
    let validConfigDirs = map (</> ".hawk") $ takeWhile (not . null)
                                            $ iterate (init . dropFileName) startDir
    foldM (maybe validDirOrNothing (const . return . Just)) Nothing validConfigDirs
  where
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
    maybeProjectConfigDir <- findContextFromCurrDir
    case maybeProjectConfigDir of
      Nothing -> getDefaultConfigDir
      Just projectConfigDir -> return projectConfigDir

-- | Check if a directory is a valid context and return true if the directory
-- doesn't exist and the parent has the right permissions
checkContextDir :: (MonadIO m) => FilePath -> UncertainT m Bool
checkContextDir dir = do
    fileExists <- io $ doesFileExist dir
    when fileExists $ fail $ concat [
       "config directory '",dir,"' cannot be"
      ,"created because a file with the same"
      ,"name exists"]
    dirExists <- io $ doesDirectoryExist dir
    if dirExists
      then do
        permissions <- io $ getPermissions dir
        when (not $ writable permissions) $ fail $ concat [
           "cannot use '",dir,"' as config directory because it is not "
          ,"writable"]
        when (not $ searchable permissions) $ fail $ concat [
           "cannot use '",dir,"' as config directory because it is not "
          ,"searchable"]
        return False
      else do
        -- if the directory doesn't exist then its parent must be writable
        -- and searchable
        let parent = case takeDirectory dir of {"" -> ".";p -> p}
        permissions <- io $ getPermissions parent
        when (not $ writable permissions) $ fail $ concat[
           "cannot create config directory '",dir,"' because the parent "
          ," directory is not writable (",show permissions,")"]
        when (not $ searchable permissions) $ fail $ concat[
           "cannot create config directory '",dir,"' because the parent "
          ," directory is not searchable (",show permissions,")"]
        warn $ concat ["directory '",dir,"' doesn't exist, creating a "
                      ,"default one"]
        return True
  where
    io = lift . liftIO
