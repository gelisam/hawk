-- | The user prelude you get if the user doesn't have a prelude.
module System.Console.Hawk.UserPrelude.Defaults where

import           Control.Arrow      ((&&&))

import           Data.HaskellModule


-- | Imported at runtime even if missing from the user prelude.
--   Since they are fully qualified, they should not conflict with any
--   user-imported module.
defaultModules :: [QualifiedModule]
defaultModules =
    -- types used to describe the expression interpreted by hint
    -- must be imported unqualified
    ("System.Console.Hawk.Runtime.Base", Nothing)
    : map fullyQualified
      [ "Prelude"
      , "System.Console.Hawk.Representable"
      , "System.Console.Hawk.Runtime"
      , "System.IO.Unsafe"
      , "Data.ByteString.Lazy.Char8"
      ]
  where
    fullyQualified = id &&& Just

defaultPrelude :: String
defaultPrelude = unlines
    [ "{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}"
    , "import Prelude"
    , "import qualified Data.ByteString.Lazy.Char8 as B"
    , "import qualified Data.List as L"
    ]

defaultModuleName :: String
defaultModuleName = "System.Console.Hawk.CachedPrelude"
