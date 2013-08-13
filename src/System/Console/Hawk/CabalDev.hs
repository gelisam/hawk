-- Extra steps to be performed if hawk was installed from cabal-dev.
-- 
-- Extra steps are needed because the hawk binary needs runtime access
-- to the hawk library, but the hint library only knows about the globally-
-- installed libraries. If hawk has been installed with cabal-dev, its
-- binary and its library will be installed in a local folder instead of
-- in the global location.
module System.Console.Hawk.CabalDev (runHawkInterpreter) where

import Data.Functor
import Data.List
import Language.Haskell.Interpreter (InterpreterT, InterpreterError, runInterpreter)
import Language.Haskell.Interpreter.Unsafe (unsafeRunInterpreterWithArgs)
import System.Directory (getDirectoryContents)
import System.Environment (getExecutablePath)
import Text.Printf (printf)


-- if hawk has been compiled by cabal-dev,
-- its binary has been placed in a cabal-dev folder
isCabalDev :: IO Bool
isCabalDev = do
    exe <- getExecutablePath
    return $ "cabal-dev/bin/hawk" `isSuffixOf` exe

-- something like "packages-7.6.3.conf"
isPackageFile :: String -> Bool
isPackageFile xs = "packages-" `isPrefixOf` xs && ".conf" `isSuffixOf` xs

-- > cabalDevDir "/.../cabal-dev/bin/hawk"
-- "/.../cabal-dev"
cabalDevDir :: String -> String
cabalDevDir exe = take (length exe - length suffix) exe where
  suffix :: String
  suffix = "/bin/hawk"

-- something like "/.../cabal-dev/package-7.6.3.conf"
cabalDevPackageFile :: IO String
cabalDevPackageFile = do
    dir <- cabalDevDir <$> getExecutablePath
    files <- getDirectoryContents dir
    let [file] = filter isPackageFile files
    return $ printf "%s/%s" dir file

-- a version of runInterpreter which can load libraries
-- installed along hawk's cabal-dev folder, if applicable.
runHawkInterpreter :: InterpreterT IO a -> IO (Either InterpreterError a)
runHawkInterpreter mx = do
    cabalDev <- isCabalDev
    if cabalDev
      then do packageFile <- cabalDevPackageFile
              let arg = printf "-package-db %s" packageFile
              unsafeRunInterpreterWithArgs [arg] mx
      else runInterpreter mx
