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
-- 
-- 
-- The second example, which adds `takeLast` to the user prelude:
-- 
-- >>> testCustomPrelude "readme" ["-a"] "takeLast 3" "0-100"
-- 98
-- 99
-- 100
testCustomPrelude :: FilePath -> [String] -> String -> FilePath -> IO ()
testCustomPrelude preludeBasename flags expr inputBasename = processArgs args
  where
    args = ["-c", preludePath] ++ flags ++ [expr, inputPath]
    preludePath = "tests" </> "preludes" </> preludeBasename
    inputPath = "tests" </> "inputs" </> inputBasename
