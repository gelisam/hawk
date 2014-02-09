-- | Tests which require particular prelude files.
module System.Console.Hawk.PreludeTests where

import System.FilePath

import System.Console.Hawk


-- |
-- 
-- The first example from the README:
-- 
-- >>> testCustomPrelude "" ["-d:", "-m"] "head" "passwd"
-- root
-- 
-- 
-- The second example, which adds `takeLast` to the user prelude:
-- 
-- >>> testCustomPrelude "readme" ["-a"] "takeLast 3" "0-100"
-- 98
-- 99
-- 100
-- 
-- 
-- The last example, a quick test to validate that Hawk was properly installed:
-- 
-- >>> testCustomPrelude "" [] "[1..3]" ""
-- 1
-- 2
-- 3
testCustomPrelude :: FilePath -> [String] -> String -> FilePath -> IO ()
testCustomPrelude preludeBasename flags expr inputBasename = processArgs args
  where
    args = preludeArgs preludeBasename
        ++ flags
        ++ [expr]
        ++ inputArgs inputBasename
    
    preludePath f = "tests" </> "preludes" </> f
    inputPath f = "tests" </> "inputs" </> f
    
    preludeArgs "" = preludeArgs "default"
    preludeArgs f = ["-c", preludePath f]
    
    inputArgs "" = []
    inputArgs f = [inputPath f]
