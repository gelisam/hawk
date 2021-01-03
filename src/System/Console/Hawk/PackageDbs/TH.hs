module System.Console.Hawk.PackageDbs.TH
    ( compileTimeEnvVar
    ) where

import Language.Haskell.TH.Syntax (TExp(TExp), Q, lift, runIO)
import System.Environment (getEnvironment)


compileTimeEnvVar :: String -> Q (TExp (Maybe String))
compileTimeEnvVar varname = do
  env <- runIO getEnvironment
  let r :: Maybe String
      r = lookup varname env
  TExp <$> lift r
