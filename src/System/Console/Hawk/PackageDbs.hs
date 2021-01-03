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
{-# LANGUAGE CPP, LambdaCase, TemplateHaskell, TupleSections #-}
module System.Console.Hawk.PackageDbs
    ( extraGhcArgs
    , runHawkInterpreter
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer
import Data.Foldable
import Data.List.Extra (wordsBy)
import Language.Haskell.Interpreter (InterpreterError, InterpreterT)
import Language.Haskell.Interpreter.Unsafe (unsafeRunInterpreterWithArgs)
import System.Directory.PathFinder
import System.FilePath (splitDirectories)
import Text.Printf (printf)

import System.Console.Hawk.PackageDbs.TH
import System.Console.Hawk.Path (getInstallationPath)


data InstallationMethod a = InstallationMethod
  { installationMethodName :: String
  , installationMethodData :: a
  }

type InstallationMethodChecker = IO (Maybe PackageDbFinder)
type PackageDbFinder = IO [FilePath]

supportedInstallationMethods :: [InstallationMethod InstallationMethodChecker]
supportedInstallationMethods
  = [ InstallationMethod "stack" $ do
        pure $ do
          haskellPackageSandboxes <- $$(compileTimeEnvVar "HASKELL_PACKAGE_SANDBOXES")
          pure $ do
            pure $ wordsBy (== ':') haskellPackageSandboxes
    , InstallationMethod "cabal v2-run" $ do  -- also used by cabal v2-test
        maybePaths <- runMaybeT $ do
          haskellDistDir <- MaybeT $ pure $$(compileTimeEnvVar "HASKELL_DIST_DIR")
          distNewstyle <- MaybeT $ flip runPathFinder haskellDistDir $ do
            -- "/.../dist-newstyle/build/x86_64-osx/ghc-8.4.4/haskell-awk-1.1.1"
            relativePath ".."
            filenameIs (printf "ghc-%s" VERSION_ghc)
            -- "/.../dist-newstyle/build/x86_64-osx/ghc-8.4.4"
            relativePath ".."
            -- "/.../dist-newstyle/build/x86_64-osx"
            relativePath ".."
            filenameIs "build"
            -- "/.../dist-newstyle/build"
            relativePath ".."
            filenameIs "dist-newstyle"
            -- "/.../dist-newstyle"
          pure (haskellDistDir, distNewstyle)
        pure $ do
          (haskellDistDir, distNewstyle) <- maybePaths
          pure $ do
            localPackageDb <- findSinglePackageDb distNewstyle $ do
              -- "/.../dist-newstyle"
              relativePath "packagedb"
              -- "/.../dist-newstyle/packagedb"
              relativePath (printf "ghc-%s" VERSION_ghc)
              -- "/.../dist-newstyle/packagedb/ghc-8.4.4"
            inPlacePackageDb <- findSinglePackageDb haskellDistDir $ do
              -- "/.../dist-newstyle/build/x86_64-osx/ghc-8.4.4/haskell-awk-1.1.1"
              relativePath "package.conf.inplace"
              -- "/.../dist-newstyle/build/x86_64-osx/ghc-8.4.4/haskell-awk-1.1.1/package.conf.inplace"
            pure [localPackageDb, inPlacePackageDb]
    , InstallationMethod "cabal v2-install" $ do
        bindir <- getInstallationPath
        maybeDotCabal <- flip runPathFinder bindir $ do
          -- "~/.cabal/store/ghc-8.4.4/hskll-wk-1.1.1-f21ccfdf/bin"
          relativePath ".."
          -- "~/.cabal/store/ghc-8.4.4/hskll-wk-1.1.1-f21ccfdf"
          relativePath ".."
          filenameIs (printf "ghc-%s" VERSION_ghc)
          -- "~/.cabal/store/ghc-8.4.4"
          relativePath ".."
          filenameIs "store"
          -- "~/.cabal/store"
          relativePath ".."
          filenameIs ".cabal"
          -- "~/.cabal"
        pure $ do
          dotCabal <- maybeDotCabal

          -- to distinguish between v1-install and v2-install
          guard ("dist-newstyle" `elem` splitDirectories $$(compileTimeWorkingDirectory))

          pure $ do
            globalPackageDb <- findSinglePackageDb dotCabal $ do
              -- "~/.cabal"
              relativePath "store"
              -- "~/.cabal/store"
              relativePath (printf "ghc-%s" VERSION_ghc)
              -- "~/.cabal/store/ghc-8.4.4"
              relativePath "package.db"
              -- "~/.cabal/store/ghc-8.4.4/package.db"
            pure [globalPackageDb]
    , InstallationMethod "cabal v1-sandbox" $ do
        bindir <- getInstallationPath
        maybeCabalSandbox <- flip runPathFinder bindir $ do
          -- "/.../haskell-awk/.cabal-sandbox/bin"
          relativePath ".."
          filenameIs ".cabal-sandbox"
          -- "/.../haskell-awk/.cabal-sandbox"
        pure $ do
          cabalSandbox <- maybeCabalSandbox
          pure $ do
            packageDb <- findSinglePackageDb cabalSandbox $ do
              -- "/.../haskell-awk/.cabal-sandbox"
              someChild
              filenameMatches "" (printf "-ghc-%s-packages.conf.d" VERSION_ghc)
              -- "/.../haskell-awk/.cabal-sandbox/x86_64-osx-ghc-8.4.4-packages.conf.d"
            pure [packageDb]
    , InstallationMethod "cabal v1-install" $ do
        bindir <- getInstallationPath
        maybeDotGhc <- flip runPathFinder bindir $ do
          -- "~/.cabal/bin"
          relativePath ".."
          filenameIs ".cabal"
          -- "~/.cabal"
          relativePath ".."
          -- "~"
          relativePath ".ghc"
          -- "~/.ghc"
        maybeDist <- flip runPathFinder $$(compileTimeWorkingDirectory) $ do
          -- "/.../haskell-awk"
          relativePath "dist"
          -- "/.../haskell-awk/dist"
        pure $ do
          dotGhc <- maybeDotGhc

          -- to distinguish between v1-install and v2-install
          _ <- maybeDist

          -- to distinguish between v1-install and v2-run
          haskellDistDir <- $$(compileTimeEnvVar "HASKELL_DIST_DIR")
          guard (haskellDistDir == "dist")

          pure $ do
            packageDb <- findSinglePackageDb dotGhc $ do
              -- "~/.ghc"
              someChild
              filenameMatches "" (printf "-%s" VERSION_ghc)
              -- "~/.ghc/x86_64-darwin-8.4.4"
              relativePath "package.conf.d"
              -- "~/.ghc/x86_64-darwin-8.4.4/package.conf.d"
            pure [packageDb]
    ]

findSinglePackageDb :: FilePath -> MultiPathFinder -> IO FilePath
findSinglePackageDb searchPath multiPathFinder = do
  runMultiPathFinder multiPathFinder searchPath >>= \case
    [] -> do
      error $ "No package database found in " ++ searchPath
    [packageDb] -> do
      pure packageDb
    packageDbs -> do
      error $ "Multiple package databases found in " ++ searchPath
        ++ ": " ++ multiPhrase "and" packageDbs
        ++ ". It is ambiguous which one to use."

-- |
-- >>> unsupportedInstallationMethodMessage
-- "Please install hawk using stack, cabal v2-run, cabal v2-install, cabal v1-sandbox, or cabal v1-install. Hawk doesn't know how to find the package database using your chosen installation method."
unsupportedInstallationMethodMessage :: String
unsupportedInstallationMethodMessage
  = "Please install hawk using "
 ++ multiPhrase "or" (fmap installationMethodName supportedInstallationMethods)
 ++ ". Hawk doesn't know how to find the package database using your chosen installation method."

multiPhrase :: String -> [String] -> String
multiPhrase connective = \case
  [x]
    -> x
  [x,y]
    -> x ++ " " ++ connective ++ " " ++ y
  xs
    -> concat (fmap (++ ", ") (init xs))
    ++ connective
    ++ " " ++ last xs


detectInstallationMethods :: IO [InstallationMethod PackageDbFinder]
detectInstallationMethods = execWriterT $ do
  for_ supportedInstallationMethods $ \(InstallationMethod name detect) -> do
    liftIO detect >>= \case
      Just packageDbFinder -> do
        tell [InstallationMethod name packageDbFinder]
      Nothing -> do
        pure ()

detectInstallationMethod :: IO (InstallationMethod PackageDbFinder)
detectInstallationMethod = do
  detectInstallationMethods >>= \case
    [] -> do
      error unsupportedInstallationMethodMessage
    [x] -> do
      pure x
    installationMethods -> do
      error $ "Multiple installation methods detected: "
           ++ multiPhrase "and" (fmap installationMethodName installationMethods)
           ++ ". It is ambiguous which package database to use."

detectPackageDbs :: IO [FilePath]
detectPackageDbs = do
  InstallationMethod _ findPackageDbs <- detectInstallationMethod
  findPackageDbs


-- something like ["-package-db /.../haskell-awk/cabal-dev/package-7.6.3.conf"]
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
