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
{-# LANGUAGE TemplateHaskell, TupleSections #-}
module System.Console.Hawk.Sandbox
    ( extraGhcArgs
    , runHawkInterpreter
    ) where

import Control.Applicative
import Control.Monad
import Data.List.Extra (wordsBy)
import Data.Maybe
import Language.Haskell.Interpreter (InterpreterT, InterpreterError)
import Language.Haskell.Interpreter.Unsafe (unsafeRunInterpreterWithArgs)
import Language.Haskell.TH.Syntax (lift, runIO)
import System.Directory.PathFinder
import System.Environment (getEnvironment)
import System.FilePath ((</>))
import Text.Printf (printf)

import System.Console.Hawk.Path (getInstallationPath)


data Sandbox = Sandbox
  { sandboxPathFinder :: PathFinder
  , packageDbFinder :: MultiPathFinder
  }

dotCabal :: Sandbox
dotCabal = Sandbox (basenameIs ".cabal") $ do
    relativePath (".." </> ".ghc")
    someChild
    relativePath "package.conf.d"

cabalSandbox :: Sandbox
cabalSandbox = Sandbox (basenameIs ".cabal-sandbox") $ do
    someChild
    basenameMatches "" "-packages.conf.d"

-- All the sandbox systems we support.
-- We also support stack and cabal-dev, via HASKELL_PACKAGE_SANDBOXES.
sandboxes :: [Sandbox]
sandboxes = [dotCabal, cabalSandbox]


-- something like (Just "/.../.cabal-sandbox")
findSandboxPath :: Sandbox -> IO (Maybe FilePath)
findSandboxPath sandbox = do
    bindir <- getInstallationPath
    let sandboxPathFromBin = relativePath ".." >> sandboxPathFinder sandbox
    runPathFinder sandboxPathFromBin bindir

-- something like (cabalSandbox, "/.../.cabal-sandbox")
detectCabalSandbox :: IO (Sandbox, FilePath)
detectCabalSandbox = do
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
detectCabalPackageDb :: IO String
detectCabalPackageDb = do
    (sandbox, sandboxPath) <- detectCabalSandbox
    let fail' s = error $ printf "%s found in sandbox %s" s sandboxPath
    packageDbPaths <- runMultiPathFinder (packageDbFinder sandbox) sandboxPath
    case packageDbPaths of
        [packageDb] -> return packageDb
        []          -> fail' "no package-db"
        _           -> fail' "multiple package-db's"

-- stack requires two package-databases, the second is passed at compile time
-- via an environment variable.
detectEnvPackageDbs :: Maybe [String]
detectEnvPackageDbs = $(do
      env <- runIO getEnvironment
      lift $ ((:[]) <$> lookup "HASKELL_DIST_DIR" env)
         <|> (wordsBy (== ':') <$> lookup "HASKELL_PACKAGE_SANDBOXES" env)
    )

-- prefer the env-provided list of package-dbs if there is one, otherwise
-- try to pick a package-db path based on the installation path given by cabal.
detectPackageDbs :: IO [String]
detectPackageDbs = case detectEnvPackageDbs of
    Just packageDbs -> return packageDbs
    Nothing -> do
      packageDb <- detectCabalPackageDb
      return [packageDb]


-- something like ["-package-db /.../cabal-dev/package-7.6.3.conf"]
extraGhcArgs :: IO [String]
extraGhcArgs = fmap (printf "-package-db %s") <$> detectPackageDbs

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
