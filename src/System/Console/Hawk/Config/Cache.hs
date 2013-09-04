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
module System.Console.Hawk.Config.Cache
    ( getConfigDir
    , getConfigFile
    , getCacheDir
    , getConfigInfosFile
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

getExtensionsFile :: IO FilePath
getExtensionsFile = getCacheDir <//> "extensions"


getSourceBasename :: IO String
getSourceBasename = getCacheDir <//> "cached_prelude"

getCompiledFile :: IO String
getCompiledFile = getSourceBasename

getSourceFile :: IO String
getSourceFile = (++ ".hs") <$> getSourceBasename


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
