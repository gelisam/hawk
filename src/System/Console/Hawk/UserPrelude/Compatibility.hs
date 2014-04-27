--   Copyright 2013 Mario Pastorelli (pastorelli.mario@gmail.com) Samuel Gélineau (gelisam@gmail.com)
--
--   Licensed under the Apache License, Version 2.0 (the "License");
--   you may not use this file except in compliance with the License.
--   You may obtain a copy of the License at
--
--       http://www.apache.org/licenses/LICENSE-2.0
--
--   Unless required by applicable law or agreed to in writing, software
--   distributed under the License is distributed on an "AS IS" BASIS,
--   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--   See the License for the specific language governing permissions and
--   limitations under the License.

{-# LANGUAGE OverloadedStrings #-}
-- | An essential part of Hawk's configuration is the user prelude.
module System.Console.Hawk.UserPrelude.Compatibility (
      defaultModules
    , defaultPrelude
    , recompileUserPrelude
    , recompileUserPrelude'
) where

import Control.Arrow ((&&&))
import Control.Monad (when)

import System.EasyFile

import System.Console.Hawk.UserPrelude.Base
import System.Console.Hawk.UserPrelude.Cache
import System.Console.Hawk.UserPrelude.Compile
import System.Console.Hawk.UserPrelude.Compatibility.Extend
import System.Console.Hawk.UserPrelude.Defaults
import System.Console.Hawk.UserPrelude.Parse


-- --
-- From now the code is heavy, it needs a refactoring (renaming)

-- | The path to the (cleaned-up) prelude file, and its module name.
--   We need both in order for hint to import its contents.
-- 
-- TODO: error handling
recompileUserPrelude :: FilePath -> IO (String,String)
recompileUserPrelude contextDir
  = recompileUserPrelude' (getUserPreludeFile contextDir)
                          (getCacheDir        contextDir)
                          (getSourceFile      contextDir)
                          (getCompiledFile    contextDir)

recompileUserPrelude' :: FilePath -- ^ prelude file
                      -> FilePath -- ^ cache dir
                      -> FilePath -- ^ source file
                      -> FilePath -- ^ output compiled file
                      -> IO (String,String)
recompileUserPrelude' preludeFile
                      cacheDir
                      sourceFile
                      compiledFile = do
    clean
    createDirectoryIfMissing True cacheDir
    
    extensions <- readExtensions preludeFile
    orig_modules <- readModules extensions preludeFile
    orig_source <- readSource preludeFile
    
    let source = extendSource preludeFile extensions orig_modules orig_source
    
    cacheSource sourceFile source
    
    compile sourceFile compiledFile cacheDir
    
    let moduleName = getModuleName source
    
    return (sourceFile, moduleName)
  where
    clean :: IO ()
    clean = do
        dirExists <- doesDirectoryExist cacheDir
        when (dirExists) (removeDirectoryRecursive cacheDir)
