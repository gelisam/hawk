{-# LANGUAGE OverloadedStrings #-}
module System.Console.Hawk.Config.Cache
    ( getConfigDir
    , getConfigFile
    , getCacheDir
    , getConfigInfosFile
    , getExtensionsFile
    , getModulesFile
    , cacheExtensions
    , cacheModules
    )
  where

import Control.Applicative ((<$>))

import qualified Language.Haskell.Interpreter as Interpreter
import System.EasyFile

import System.Console.Hawk.Config.Parse


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


cacheExtensions :: [ExtensionName] -> IO ()
cacheExtensions extensions = do
    extensionsFile <- getExtensionsFile
    writeFile extensionsFile $ show extensions'
  where
    extensions' :: [Interpreter.Extension]
    extensions' = map read extensions

cacheModules :: [QualifiedModules] -> IO ()
cacheModules modules = do
    modulesFile <- getModulesFile
    writeFile modulesFile $ show modules
