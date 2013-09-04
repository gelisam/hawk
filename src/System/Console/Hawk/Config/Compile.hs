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
module System.Console.Hawk.Config.Compile
    ( compile
    )
  where

import Control.Monad (when)

import System.Exit
import System.IO
import System.Process


-- compile a haskell file
-- TODO: this should return the error instead of print it and exit
compile :: FilePath -- ^ the source file
        -> FilePath -- ^ the output file
        -> FilePath -- ^ the directory used for compiler files
        -> IO ()
compile sourceFile outputFile dir = do
    compExitCode <-
            waitForProcess =<< runProcess "ghc" ["--make"
                                               , sourceFile
                                               , "-i"
                                               , "-ilib"
                                               , "-fforce-recomp"
                                               , "-v0"
                                               , "-o",outputFile]
                                          (Just dir)
                                          Nothing
                                          Nothing
                                          Nothing
                                          (Just stderr)
    when (compExitCode /= ExitSuccess) $ do
        exitFailure
