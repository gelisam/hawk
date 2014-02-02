--   Copyright 2013 Mario Pastorelli (pastorelli.mario@gmail.com) Samuel Gélineau (gelisam@gmail.com)
--
--   Licensed under the Apache License, Version 2.0 (the "License");
--   you may not use this file except in compliance with the License.
--   You may obtain a copy of the License at
--
--       http://www.apache.org/licenses/LICENSE-2.0
--
--   Unless required by applicable law or agreed to in writing, software
--   distributed under the License is distributed on an "AS IS" BASIS,
--   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--   See the License for the specific language governing permissions and
--   limitations under the License.

{-# LANGUAGE OverloadedStrings #-}
-- | The user expression is interpreted, but the user prelude is compiled.
module System.Console.Hawk.UserPrelude.Compile
    ( compile
    )
  where

import Control.Monad (when)

import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process

import System.Console.Hawk.Sandbox (extraGhcArgs)


-- A version of `canonicalizePath` which works even if the file
-- doesn't exist.
absPath :: FilePath -> IO FilePath
absPath f = do
    pwd <- getCurrentDirectory
    return (pwd </> f)


-- compile a haskell file
-- TODO: this should return the error instead of print it and exit
compile :: FilePath -- ^ the source file
        -> FilePath -- ^ the output file
        -> FilePath -- ^ the directory used for compiler files
        -> IO ()
compile sourceFile outputFile dir = do
    absSource <- absPath sourceFile
    absOutput <- absPath outputFile
    let basicArgs = [ "--make"
                    , absSource
                    , "-i"
                    , "-ilib"
                    , "-fforce-recomp"
                    , "-v0"
                    , "-o",absOutput]
    extraArgs <- extraGhcArgs
    let args = basicArgs ++ extraArgs
    compExitCode <-
            waitForProcess =<< runProcess "ghc"
                                          args
                                          (Just dir)
                                          Nothing
                                          Nothing
                                          Nothing
                                          (Just stderr)
    when (compExitCode /= ExitSuccess) $ do
        exitFailure
