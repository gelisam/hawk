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

-- Extra steps to be performed if hawk was installed from a sandbox.
-- 
-- Extra steps are needed because the hawk binary needs runtime access
-- to the hawk library, but the hint library only knows about the globally-
-- installed libraries. If hawk has been installed with a sandbox, its
-- binary and its library will be installed in a local folder instead of
-- in the global location.
module System.Console.Hawk.Sandbox
    ( extraGhcArgs
    , runHawkInterpreter
    ) where

import Control.Applicative
import Control.Monad
import Data.List
import Language.Haskell.Interpreter (InterpreterT, InterpreterError)
import Language.Haskell.Interpreter.Unsafe (unsafeRunInterpreterWithArgs)
import System.Directory (getDirectoryContents, getHomeDirectory, doesFileExist)
import System.Environment (getExecutablePath)
import System.FilePath (pathSeparator, splitFileName)
import Text.Printf (printf)


data Sandbox = Sandbox
  { folder :: FilePath
  , packageFilePrefix :: String
  , packageFileSuffix :: String
  }

cabalDev, cabalSandbox :: Sandbox
cabalDev = Sandbox "cabal-dev" "packages-" ".conf"
cabalSandbox = Sandbox ".cabal-sandbox" "" "-packages.conf.d"

-- all the sandbox systems we support.
sandboxes :: [Sandbox]
sandboxes = [cabalDev, cabalSandbox]


-- a version of isSuffixOf which returns the string stripped of its suffix.
isSuffixOf' :: String -> String -> Maybe String
isSuffixOf' suffix s = if suffix `isSuffixOf` s
                         then Just (take (n - m) s)
                         else Nothing
  where
    n = length s
    m = length suffix

-- a version of doesFileExist which returns the file path if it exists.
doesFileExist' :: FilePath -> IO (Maybe FilePath)
doesFileExist' f = do
    r <- doesFileExist f
    if r
      then return (Just f)
      else return Nothing

firstWhichExists :: [FilePath] -> IO (Maybe FilePath)
firstWhichExists fs = do
    fs' <- mapM doesFileExist' fs
    return $ msum fs'


-- convert slashes to backslashes if needed
path :: String -> String
path = map replaceSeparator where
  replaceSeparator '/' = pathSeparator
  replaceSeparator x = x

-- a version of getExecutablePath which returns the path to the installed hawk
-- executable even the current executable is actually hawk's test suite.
getHawkPath :: IO FilePath
getHawkPath = do
    executablePath <- getExecutablePath
    case path "/dist/build/reference/reference" `isSuffixOf'` executablePath of
      Nothing -> return executablePath
      Just basePath -> do
        -- We are running the test suite. Is hawk installed?
        
        home <- getHomeDirectory
        let userPath = home ++ "/.cabal/bin/hawk"
        
        let folders = map folder sandboxes
        let sandboxPaths = map (printf "%s/%s/bin/hawk" basePath) folders
        
        firstMatch <- firstWhichExists (userPath:sandboxPaths)
        case firstMatch of
          Nothing -> fail "please run 'cabal install' before 'cabal test'."
          Just hawkPath -> return hawkPath


-- if hawk has been compiled by a sandboxing tool,
-- its binary has been placed in a special folder.
-- 
-- return something like (Just "/.../cabal-dev")
getSandboxDir :: Sandbox -> IO (Maybe String)
getSandboxDir sandbox = do
    (dir, _) <- splitFileName <$> getHawkPath
    let sandboxFolder = folder sandbox
    let suffix = path (sandboxFolder ++ "/bin/")
    let basePath = suffix `isSuffixOf'` dir
    let sandboxPath = fmap (++ sandboxFolder) basePath
    return sandboxPath

-- something like "packages-7.6.3.conf"
isPackageFile :: Sandbox -> FilePath -> Bool
isPackageFile sandbox f = packageFilePrefix sandbox `isPrefixOf` f
                       && packageFileSuffix sandbox `isSuffixOf` f

-- something like "/.../cabal-dev/package-7.6.3.conf"
getPackageFile :: Sandbox -> String -> IO String
getPackageFile sandbox dir = do
    files <- getDirectoryContents dir
    case filter (isPackageFile sandbox) files of
      [file] -> return $ printf (path "%s/%s") dir file
      [] -> fail' "no package-db"
      _ -> fail' $ "multiple package-db's"
  where
    fail' s = error $ printf "%s found in sandbox %s" s (folder sandbox)

sandboxSpecificGhcArgs :: Sandbox -> IO [String]
sandboxSpecificGhcArgs sandbox = do
    sandboxDir <- getSandboxDir sandbox
    case sandboxDir of
      Nothing -> return []
      Just dir -> do packageFile <- getPackageFile sandbox dir
                     let arg = printf "-package-db %s" packageFile
                     return [arg]


extraGhcArgs :: IO [String]
extraGhcArgs = concat <$> mapM sandboxSpecificGhcArgs sandboxes

-- a version of runInterpreter which can load libraries
-- installed along hawk's sandbox folder, if applicable.
runHawkInterpreter :: InterpreterT IO a -> IO (Either InterpreterError a)
runHawkInterpreter mx = do
    args <- extraGhcArgs
    unsafeRunInterpreterWithArgs args mx
