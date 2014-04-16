-- | Convert new-style Context to old-style Config.
module System.Console.Hawk.Context.Compatibility where

import Control.Arrow

import System.Console.Hawk.Context.Base


-- cleaned-up prelude file, module name
type Config = (String, String)


configFromContext :: Context -> Config
configFromContext = canonicalPrelude &&& moduleName
