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
-- | As the user tunes his expression, hawk's loading time gets in the way.
--   To shorten it, we cache the information we need from the user prelude.
module System.Console.Hawk.Config.Cache
    ( getConfigDir
    , getConfigFile
    , getCacheDir
    , getConfigInfosFile
    , getEvalContextFile
    , getExtensionsFile
    , getModulesFile
    , getCompiledFile
    , getSourceFile
    , cacheExtensions
    , cacheModules
    , cacheSource
    )
  where

import Control.Applicative ((<$>))

import qualified Data.ByteString.Char8 as B
import qualified Language.Haskell.Interpreter as Interpreter
import System.EasyFile

import System.Console.Hawk.Config.Base


-- | Looks less awkward on the right.
-- 
-- >>> return "myfolder" <//> "myfile.txt"
-- "myfolder/myfile.txt"
(<//>) :: IO FilePath -> FilePath -> IO FilePath
lpath <//> rpath = (</> rpath) <$> lpath

-- | Looks less awkward on the right.
-- 
-- >>> return "myfile" <++> ".txt"
-- "myfile.txt"
(<++>) :: IO String -> String -> IO String
lstr <++> rstr = (++ rstr) <$> lstr


getConfigDir :: IO FilePath
getConfigDir = getHomeDirectory <//> ".hawk"

getConfigFile :: IO FilePath
getConfigFile = getConfigDir <//> "prelude.hs"

getCacheDir :: IO FilePath
getCacheDir = getConfigDir <//> "cache"

getConfigInfosFile :: IO FilePath
getConfigInfosFile = getCacheDir <//> "configInfos"

getEvalContextFile :: IO FilePath
getEvalContextFile = getCacheDir <//> "evalContext"

getModulesFile :: IO FilePath
getModulesFile = getCacheDir <//> "modules"

getExtensionsFile :: IO FilePath
getExtensionsFile = getCacheDir <//> "extensions"


getSourceBasename :: IO String
getSourceBasename = getCacheDir <//> "cached_prelude"

getCompiledFile :: IO String
getCompiledFile = getSourceBasename

getSourceFile :: IO String
getSourceFile = getSourceBasename <++> ".hs"


cacheExtensions :: FilePath
                -> [ExtensionName]
                -> IO ()
cacheExtensions extensionsFile extensions = 
  writeFile extensionsFile $ show extensions'
  where
    extensions' :: [Interpreter.Extension]
    extensions' = map read extensions

cacheModules :: FilePath
             -> [QualifiedModule]
             -> IO ()
cacheModules modulesFile modules = writeFile modulesFile $ show modules

cacheSource :: FilePath
            -> Source
            -> IO ()
cacheSource = B.writeFile
