-- | Auto-detects the current version, used by `hawk --version`.
module System.Console.Hawk.Version (versionString) where

import Data.List (intercalate)
import Data.Version (versionBranch)

-- magic self-referential module created by cabal
import Paths_haskell_awk (version)


-- | Something like "1.0"
versionString :: String
versionString = intercalate "."
              $ map show
              $ versionBranch version
