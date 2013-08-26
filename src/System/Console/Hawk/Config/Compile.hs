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
