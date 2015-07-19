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
{-# LANGUAGE TupleSections #-}
module System.Console.Hawk.Sandbox
    ( extraGhcArgs
    , runHawkInterpreter
    ) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Language.Haskell.Interpreter (InterpreterT, InterpreterError)
import Language.Haskell.Interpreter.Unsafe (unsafeRunInterpreterWithArgs)
import System.Directory.PathFinder
import System.FilePath ((</>))
import Text.Printf (printf)

-- magic self-referential module created by cabal
import Paths_haskell_awk (getBinDir)


data Sandbox = Sandbox
  { sandboxPathFinder :: PathFinder
  , packageDbFinder :: MultiPathFinder
  }

cabalDev :: Sandbox
cabalDev = Sandbox (basenameIs "cabal-dev") $ do
    someChild
    basenameMatches "packages-" ".conf"

dotCabal :: Sandbox
dotCabal = Sandbox (basenameIs ".cabal") $ do
    relativePath (".." </> ".ghc")
    someChild
    relativePath "package.conf.d"

cabalSandbox :: Sandbox
cabalSandbox = Sandbox (basenameIs ".cabal-sandbox") $ do
    someChild
    basenameMatches "" "-packages.conf.d"

stackTool :: Sandbox
stackTool = Sandbox (hasAncestor ".stack-work") $ do
    relativePath "pkgdb"

-- all the sandbox systems we support.
sandboxes :: [Sandbox]
sandboxes = [cabalDev, dotCabal, cabalSandbox, stackTool]


-- something like (Just "/.../.cabal-sandbox")
findSandboxPath :: Sandbox -> IO (Maybe FilePath)
findSandboxPath sandbox = do
    bindir <- Paths_haskell_awk.getBinDir
    let sandboxPathFromBin = relativePath ".." >> sandboxPathFinder sandbox
    runPathFinder sandboxPathFromBin bindir

-- something like (cabalSandbox, "/.../.cabal-sandbox")
detectSandbox :: IO (Sandbox, FilePath)
detectSandbox = do
    detectedSandboxes <- forM sandboxes $ \sandbox -> do
        sandboxPath <- findSandboxPath sandbox
        return $ (sandbox,) <$> sandboxPath
    case catMaybes detectedSandboxes of
      [r] -> return r
      []  -> error "No package-db found. Did you install Hawk in an unusual way?"
      rs  -> let paths = fmap snd rs
                 msg = printf "Multiple sandboxes found: %s\nDon't know which one to use, aborting."
                              (show paths)
             in error msg

-- something like "/.../cabal-dev/package-7.6.3.conf"
detectPackageDb :: IO String
detectPackageDb = do
    (sandbox, sandboxPath) <- detectSandbox
    let fail' s = error $ printf "%s found in sandbox %s" s sandboxPath
    packageDbPaths <- runMultiPathFinder (packageDbFinder sandbox) sandboxPath
    case packageDbPaths of
        [packageDb] -> return packageDb
        []          -> fail' "no package-db"
        _           -> fail' "multiple package-db's"


-- something like ["-package-db /.../cabal-dev/package-7.6.3.conf"]
extraGhcArgs :: IO [String]
extraGhcArgs = do
    packageDb <- detectPackageDb
    return [printf "-package-db %s" packageDb]

-- | a version of runInterpreter which can load libraries
--   installed along hawk's sandbox folder, if applicable.
-- 
-- Must be called inside a `withLock` block, otherwise hint will generate
-- conflicting temporary files.
-- 
-- TODO: Didn't we write a patch for hint about this?
--       Do we still need the lock?
runHawkInterpreter :: InterpreterT IO a -> IO (Either InterpreterError a)
runHawkInterpreter mx = do
    args <- extraGhcArgs
    unsafeRunInterpreterWithArgs args mx
