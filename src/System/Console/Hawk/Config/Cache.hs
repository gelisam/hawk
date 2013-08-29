{-# LANGUAGE ImplicitParams, OverloadedStrings #-}
module System.Console.Hawk.Config.Cache
    ( getConfigDir
    , getConfigFile
    , getCacheDir
    , getConfigInfosFile
    , getExtensionsFile
    , getModulesFile
    , getCompiledFile
    , getSourceFile
    , defaultModuleName
    , cacheExtensions
    , cacheModules
    , cacheSource
    )
  where

import Control.Applicative ((<$>))

import qualified Data.ByteString.Char8 as B
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


getSourceBasename :: (?frozenTime :: String) => IO String
getSourceBasename = getCacheDir <//> ("config" ++ ?frozenTime)

getCompiledFile :: (?frozenTime :: String) => IO String
getCompiledFile = getSourceBasename

getSourceFile :: (?frozenTime :: String) => IO String
getSourceFile = (++ ".hs") <$> getSourceBasename

defaultModuleName :: (?frozenTime :: String) => String
defaultModuleName = "Hawk.M" ++ ?frozenTime


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

cacheSource :: (?frozenTime :: String) => B.ByteString -> IO ()
cacheSource source = do
    sourceFile <- getSourceFile
    B.writeFile sourceFile source
