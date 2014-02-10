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
module System.Console.Hawk.UserPrelude.Cache
    ( getDefaultContextDir
    , getUserPreludeFile
    , getCacheDir
    , getContextFile
    , getCompiledFile
    , getSourceFile
    , cacheSource
    )
  where

import Control.Applicative ((<$>))

import qualified Data.ByteString.Char8 as B
import System.EasyFile

import System.Console.Hawk.UserPrelude.Base


-- | Looks less awkward on the right.
-- 
-- >>> return "myfolder" <//> "myfile.txt"
-- "myfolder/myfile.txt"
(<//>) :: IO FilePath -> FilePath -> IO FilePath
lpath <//> rpath = (</> rpath) <$> lpath

-- | Default context directory
getDefaultContextDir :: IO FilePath
getDefaultContextDir = getHomeDirectory <//> ".hawk"

getUserPreludeFile :: FilePath -> FilePath
getUserPreludeFile = (</> "prelude.hs")

getCacheDir :: FilePath -> FilePath
getCacheDir = (</> "cache")

getContextFile :: FilePath -> FilePath
getContextFile = (</> "context") . getCacheDir


getSourceBasename :: FilePath -> String
getSourceBasename = (</> "cached_prelude") . getCacheDir

getCompiledFile :: FilePath -> String
getCompiledFile = getSourceBasename

getSourceFile :: FilePath -> String
getSourceFile = (++ ".hs") . getSourceBasename


cacheSource :: FilePath
            -> Source
            -> IO ()
cacheSource = B.writeFile
