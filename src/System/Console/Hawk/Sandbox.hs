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

-- Extra steps to be performed if hawk was installed from cabal-dev.
-- 
-- Extra steps are needed because the hawk binary needs runtime access
-- to the hawk library, but the hint library only knows about the globally-
-- installed libraries. If hawk has been installed with cabal-dev, its
-- binary and its library will be installed in a local folder instead of
-- in the global location.
module System.Console.Hawk.Sandbox
    ( extraGhcArgs
    , runHawkInterpreter
    ) where

import Data.Functor
import Data.List
import Language.Haskell.Interpreter (InterpreterT, InterpreterError)
import Language.Haskell.Interpreter.Unsafe (unsafeRunInterpreterWithArgs)
import System.Directory (getDirectoryContents)
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


-- convert slashes to backslashes if needed
path :: String -> String
path = map replaceSeparator where
  replaceSeparator '/' = pathSeparator
  replaceSeparator x = x

-- if hawk has been compiled by a sandboxing tool,
-- its binary has been placed in a special folder.
-- 
-- return something like (Just "/.../cabal-dev")
sandboxDir :: Sandbox -> IO (Maybe String)
sandboxDir sandbox = do
    (dir, _) <- splitFileName <$> getExecutablePath
    let suffix = folder sandbox ++ "/bin/"
    if path suffix `isSuffixOf` dir
      then return $ Just $ take (length dir - length "/bin/") dir
      else return $ Nothing

-- something like "packages-7.6.3.conf"
isCabalDevPackageFile :: String -> Bool
isCabalDevPackageFile xs = "packages-" `isPrefixOf` xs
                        && ".conf" `isSuffixOf` xs

-- something like "x86_64-osx-ghc-7.6.3-packages.conf.d"
isCabalSandboxPackageFile :: String -> Bool
isCabalSandboxPackageFile xs = "-packages.conf.d" `isSuffixOf` xs

-- something like "/.../cabal-dev/package-7.6.3.conf"
cabalDevPackageFile :: String -> IO String
cabalDevPackageFile dir = do
    files <- getDirectoryContents dir
    let [file] = filter isCabalDevPackageFile files
    return $ printf (path "%s/%s") dir file

-- something like "/.../cabal-dev/package-7.6.3.conf"
cabalSandboxPackageFile :: String -> IO String
cabalSandboxPackageFile dir = do
    files <- getDirectoryContents dir
    let [file] = filter isCabalSandboxPackageFile files
    return $ printf (path "%s/%s") dir file

extraGhcArgs :: IO [String]
extraGhcArgs = do
    cabalSandbox <- sandboxDir cabalSandbox
    case cabalSandbox of
      Nothing -> return []
      Just dir -> do packageFile <- cabalSandboxPackageFile dir
                     let arg = printf "-package-db %s" packageFile
                     return [arg]

-- a version of runInterpreter which can load libraries
-- installed along hawk's sandbox folder, if applicable.
runHawkInterpreter :: InterpreterT IO a -> IO (Either InterpreterError a)
runHawkInterpreter mx = do
    args <- extraGhcArgs
    unsafeRunInterpreterWithArgs args mx
