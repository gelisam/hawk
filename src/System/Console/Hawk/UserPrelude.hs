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
module System.Console.Hawk.UserPrelude (
      defaultModules
    , defaultPrelude
    , getExtensionsFile
    , getModulesFile
    , recompileUserPrelude
    , recompileUserPrelude'
) where

import Control.Arrow ((&&&))
import Control.Monad (when)

import System.EasyFile

import System.Console.Hawk.UserPrelude.Base
import System.Console.Hawk.UserPrelude.Cache
import System.Console.Hawk.UserPrelude.Compile
import System.Console.Hawk.UserPrelude.Extend
import System.Console.Hawk.UserPrelude.Parse


-- | Imported at runtime even if missing from the user prelude.
--   Since they are fully qualified, they should not conflict with any
--   user-imported module.
defaultModules :: [QualifiedModule]
defaultModules = map fullyQualified [ "Prelude"
                                     , "System.Console.Hawk.IO"
                                     , "System.Console.Hawk.Representable"
                                     , "System.Console.Hawk.Runtime"
                                     , "System.IO.Unsafe"
                                     , "Data.ByteString.Lazy.Char8"
                                     ]
  where
    fullyQualified = (id &&& Just)

defaultPrelude :: String
defaultPrelude = unlines
               [ "{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}"
               , "import Prelude"
               , "import qualified Data.ByteString.Lazy.Char8 as B"
               , "import qualified Data.List as L"
               ]

-- --
-- From now the code is heavy, it needs a refactoring (renaming)

-- | The path to the (cleaned-up) prelude file, and its module name.
--   We need both in order for hint to import its contents.
-- 
-- TODO: error handling
recompileUserPrelude :: FilePath -> IO (String,String)
recompileUserPrelude confDir
  = recompileUserPrelude' (getUserPreludeFile confDir)
                          (getCacheDir        confDir)
                          (getSourceFile      confDir)
                          (getExtensionsFile  confDir)
                          (getModulesFile     confDir)
                          (getCompiledFile    confDir)
                          (getConfigInfosFile confDir)

recompileUserPrelude' :: FilePath -- ^ prelude file
                      -> FilePath -- ^ cache dir
                      -> FilePath -- ^ source file
                      -> FilePath -- ^ output extensions cache file
                      -> FilePath -- ^ output modules cache file
                      -> FilePath -- ^ output compiled file
                      -> FilePath -- ^ output config info path
                      -> IO (String,String)
recompileUserPrelude' preludeFile
                      cacheDir
                      sourceFile
                      extensionsFile
                      modulesFile
                      compiledFile
                      configInfosFile = do
    clean
    createDirectoryIfMissing True cacheDir
    
    extensions <- readExtensions preludeFile
    orig_modules <- readModules extensions preludeFile
    orig_source <- readSource preludeFile
    
    let modules = extendModules extensions orig_modules
    let source = extendSource preludeFile extensions orig_modules orig_source
    
    cacheExtensions extensionsFile extensions
    cacheModules modulesFile modules
    cacheSource sourceFile source
    
    compile sourceFile compiledFile cacheDir
    
    let moduleName = getModuleName source
    lastModTime <- getModificationTime preludeFile
    writeFile configInfosFile $ unlines [sourceFile
                                         ,moduleName
                                         ,show lastModTime]
    
    return (sourceFile, moduleName)
  where
    clean :: IO ()
    clean = do
        dirExists <- doesDirectoryExist cacheDir
        when (dirExists) (removeDirectoryRecursive cacheDir)
