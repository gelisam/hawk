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
import System.FilePath (pathSeparator, splitFileName)
import Text.Printf (printf)


-- convert slashes to backslashes if needed
path :: String -> String
path = map replaceSeparator where
  replaceSeparator '/' = pathSeparator
  replaceSeparator x = x

-- if hawk has been compiled by cabal-dev,
-- its binary has been placed in a cabal-dev folder.
-- 
-- return something like (Just "/.../cabal-dev")
cabalDevDir :: IO (Maybe String)
cabalDevDir = do
    (dir, _) <- splitFileName <$> getExecutablePath
    if path "cabal-dev/bin/" `isSuffixOf` dir
      then return $ Just $ take (length dir - length "/bin/") dir
      else return $ Nothing

-- something like "packages-7.6.3.conf"
isPackageFile :: String -> Bool
isPackageFile xs = "packages-" `isPrefixOf` xs && ".conf" `isSuffixOf` xs

-- something like "/.../cabal-dev/package-7.6.3.conf"
cabalDevPackageFile :: String -> IO String
cabalDevPackageFile dir = do
    files <- getDirectoryContents dir
    let [file] = filter isPackageFile files
    return $ printf (path "%s/%s") dir file

-- a version of runInterpreter which can load libraries
-- installed along hawk's cabal-dev folder, if applicable.
runHawkInterpreter :: InterpreterT IO a -> IO (Either InterpreterError a)
runHawkInterpreter mx = do
    cabalDev <- cabalDevDir
    case cabalDev of
      Nothing -> runInterpreter mx
      Just dir -> do packageFile <- cabalDevPackageFile dir
                     let arg = printf "-package-db %s" packageFile
                     unsafeRunInterpreterWithArgs [arg] mx
