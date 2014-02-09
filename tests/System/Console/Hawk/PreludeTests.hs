-- | Tests which require particular prelude files.
module System.Console.Hawk.PreludeTests where

import System.FilePath

import System.Console.Hawk


-- |
-- 
-- The first example from the README:
-- 
-- >>> testCustomPrelude "default" ["-d:", "-m"] "head" "passwd"
-- root
testCustomPrelude :: FilePath -> [String] -> String -> FilePath -> IO ()
testCustomPrelude preludeBasename flags expr inputBasename = processArgs args
  where
    args = flags ++ [expr, inputPath]
    preludePath = "tests" </> "preludes" </> preludeBasename
    inputPath = "tests" </> "inputs" </> inputBasename
