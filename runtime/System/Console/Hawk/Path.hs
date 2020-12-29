-- | Auto-detects the installation folder, used to find the other installed packages.
module System.Console.Hawk.Path (getInstallationPath) where

-- magic self-referential module created by cabal
import Paths_haskell_awk (getBinDir)


getInstallationPath :: IO FilePath
getInstallationPath = getBinDir
