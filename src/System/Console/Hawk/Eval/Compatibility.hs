-- | Convert new-style EvalContext to old-style Config.
module System.Console.Hawk.Eval.Compatibility where

import Control.Arrow

import System.Console.Hawk.Eval.Context


-- cleaned-up prelude file, module name
type Config = (String, String)


configFromContext :: EvalContext -> Config
configFromContext = canonicalPrelude &&& moduleName
