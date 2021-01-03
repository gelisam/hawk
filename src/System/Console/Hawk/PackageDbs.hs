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

import Control.Monad.IO.Class
import Control.Monad.Trans.Writer
import Data.Foldable
import Data.List.Extra (wordsBy)
import Language.Haskell.Interpreter (InterpreterError, InterpreterT)
import Language.Haskell.Interpreter.Unsafe (unsafeRunInterpreterWithArgs)
import System.Directory.PathFinder
import Text.Printf (printf)

import System.Console.Hawk.PackageDbs.TH
import System.Console.Hawk.Path (getInstallationPath)


data InstallationMethod a = InstallationMethod
  { installationMethodName :: String
  , installationMethodData :: a
  }

type InstallationMethodChecker = IO (Maybe PackageDbFinder)
type PackageDbFinder = IO [String]

supportedInstallationMethods :: [InstallationMethod InstallationMethodChecker]
supportedInstallationMethods
  = [ InstallationMethod "stack" $ do
        pure $ do
          haskell_package_sandboxes <- $$(compileTimeEnvVar "HASKELL_PACKAGE_SANDBOXES")
          pure $ do
            pure $ wordsBy (== ':') haskell_package_sandboxes
    , InstallationMethod "cabal-v1" $ do
        bindir <- getInstallationPath
        maybeDotCabal <- flip runPathFinder bindir $ do
          -- "~/.cabal/bin"
          relativePath ".."
          filenameIs ".cabal"
          -- "~/.cabal"
        pure $ do
          dotCabal <- maybeDotCabal
          pure $ do
            packageDbs <- flip runMultiPathFinder dotCabal $ do
              -- "~/.cabal"
              relativePath ".."
              -- "~"
              relativePath ".ghc"
              -- "~/.ghc"
              someChild
              filenameMatches "" ("-" ++ VERSION_ghc)
              -- "~/.ghc/x86_64-darwin-8.4.4"
              relativePath "package.conf.d"
              -- "~/.ghc/x86_64-darwin-8.4.4/package.conf.d"
            packageDb <- singlePackageDb "~/.ghc" packageDbs
            pure [packageDb]
    , InstallationMethod "cabal-sandbox" $ do
        bindir <- getInstallationPath
        maybeCabalSandbox <- flip runPathFinder bindir $ do
          -- "/.../.cabal-sandbox/bin"
          relativePath ".."
          filenameIs ".cabal-sandbox"
          -- "/.../.cabal-sandbox"
        pure $ do
          cabalSandbox <- maybeCabalSandbox
          pure $ do
            packageDbs <- flip runMultiPathFinder cabalSandbox $ do
              -- "/.../.cabal-sandbox"
              someChild
              filenameMatches "" ("-ghc-" ++ VERSION_ghc ++ "-packages.conf.d")
              -- "/.../.cabal-sandbox/x86_64-osx-ghc-8.4.4-packages.conf.d"
            packageDb <- singlePackageDb cabalSandbox packageDbs
            pure [packageDb]
    ]

singlePackageDb :: String -> [String] -> IO String
singlePackageDb searchPath = \case
  [] -> do
    error $ "No package database found in " ++ searchPath
  [x] -> do
    pure x
  xs -> do
    error $ "Multiple package databases found in " ++ searchPath
      ++ ": " ++ multiPhrase "and" xs
      ++ ". It is ambiguous which one to use."

-- |
-- >>> unsupportedInstallationMethodMessage
-- "Please install hawk using stack, cabal-v1, or cabal-sandbox. Hawk doesn't know how to find the package database using your chosen installation method."
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

detectPackageDbs :: IO [String]
detectPackageDbs = do
  InstallationMethod _ findPackageDbs <- detectInstallationMethod
  findPackageDbs


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
