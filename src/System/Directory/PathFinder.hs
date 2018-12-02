-- | Tiny DSL for finding a path from the current path.
{-# LANGUAGE LambdaCase #-}
module System.Directory.PathFinder where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.List
import ListT (ListT)
import System.Directory
import System.FilePath
import qualified ListT


type PathFinder = StateT FilePath (MaybeT IO) ()
type MultiPathFinder = StateT FilePath (ListT IO) ()

runPathFinder :: PathFinder -> FilePath -> IO (Maybe FilePath)
runPathFinder p pwd = runMaybeT (execStateT p pwd)

runMultiPathFinder :: MultiPathFinder -> FilePath -> IO [FilePath]
runMultiPathFinder p pwd = ListT.toList (execStateT p pwd)


basenameIs :: MonadPlus m => String -> StateT FilePath m ()
basenameIs s = do
    pwd <- get
    guard (takeFileName pwd == s)

basenameMatches :: MonadPlus m => String -> String -> StateT FilePath m ()
basenameMatches prefix suffix = do
    pwd <- get
    guard (prefix `isPrefixOf` pwd && suffix `isSuffixOf` pwd)

hasAncestor :: MonadPlus m => String -> StateT FilePath m ()
hasAncestor s = do
    pwd <- get
    guard (s `elem` splitDirectories pwd)

relativePath :: (MonadIO m, MonadPlus m) => FilePath -> StateT FilePath m ()
relativePath rel = do
    pwd <- get
    let pwd' = pwd </> rel
    exists <- liftIO $ doesDirectoryExist pwd'
    guard exists
    pwd'' <- liftIO $ canonicalizePath pwd'
    put pwd''

someChild :: MultiPathFinder
someChild = do
    pwd <- get
    childs <- liftIO $ getDirectoryContents pwd
    child <- lift $ ListT.fromFoldable childs
    put (pwd </> child)
