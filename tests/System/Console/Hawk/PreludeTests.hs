-- | Tests which require particular prelude files.
module System.Console.Hawk.PreludeTests where

import System.FilePath

import System.Console.Hawk


-- |
-- >>> testCustomPrelude "default" "hawk -d: -m 'head'"
-- root
testCustomPrelude :: FilePath -> String -> IO ()
testCustomPrelude preludeBasename expr = processArgs args
  where
    args = ["-c", preludePath, expr]
    preludePath = "tests" </> "preludes" </> preludeBasename
