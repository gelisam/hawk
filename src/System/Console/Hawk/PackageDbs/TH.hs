{-# LANGUAGE CPP #-}
module System.Console.Hawk.PackageDbs.TH
    ( compileTimeEnvVar
    , compileTimeWorkingDirectory
    ) where

import Language.Haskell.TH.Syntax (TExp(TExp), Q, lift, runIO)
#if MIN_VERSION_template_haskell(2,17,0)
import Language.Haskell.TH.Syntax (Code, liftCode)
#endif
import System.Directory (getCurrentDirectory)
import System.Environment (getEnvironment)


#if !MIN_VERSION_template_haskell(2,17,0)
type Code m a = m (TExp a)

liftCode :: m (TExp a) -> Code m a
liftCode = id
#endif

compileTimeEnvVar :: String -> Code Q (Maybe String)
compileTimeEnvVar varname = liftCode $ do
  env <- runIO getEnvironment
  let r :: Maybe String
      r = lookup varname env
  TExp <$> lift r

compileTimeWorkingDirectory :: Code Q String
compileTimeWorkingDirectory = liftCode $ do
  pwd <- runIO getCurrentDirectory
  TExp <$> lift pwd
