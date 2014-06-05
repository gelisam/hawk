{-# Language OverloadedStrings #-}
-- | The user prelude you get if the user doesn't have a prelude.
module System.Console.Hawk.UserPrelude.Defaults where

import Control.Arrow ((&&&))
import qualified Data.Text.Lazy as T

import Data.HaskellModule


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
      ]
  where
    fullyQualified = (id &&& Just)

defaultPrelude :: T.Text
defaultPrelude = T.unlines
    [ "{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}"
    , "import Prelude"
    , "import qualified Data.List as L"
    ]

defaultModuleName :: T.Text
defaultModuleName = "System.Console.Hawk.CachedPrelude"
