-- | Auto-detects the current version, used by `hawk --version`.
module System.Console.Hawk.Version (version, versionString) where

import Data.List (intercalate)
import Data.Version (Version, versionBranch)

-- magic self-referential module created by cabal
import qualified Paths_haskell_awk as Paths


-- | Something like "1.0"
version :: Version
version = Paths.version

-- | Something like "1.0"
versionString :: String
versionString = intercalate "."
              $ map show
              $ versionBranch Paths.version
