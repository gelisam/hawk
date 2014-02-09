-- | Tests which require particular prelude files.
module System.Console.Hawk.PreludeTests () where

import System.FilePath

import System.Console.Hawk

-- | A helper for specifying which arguments to pass to Hawk.
--   Simplifies testing.
testBuilder :: FilePath -> FilePath -> [String] -> String -> FilePath -> IO ()
testBuilder preludeBase preludeBasename flags expr inputBasename
  = processArgs args
  where
    args = preludeArgs preludeBasename
        ++ flags
        ++ [expr]
        ++ inputArgs inputBasename
    
    preludePath f = preludeBase </> f
    inputPath f = "tests" </> "inputs" </> f
    
    preludeArgs f = ["-c", preludePath f]
    
    inputArgs "" = []
    inputArgs f = [inputPath f]

-- | A version of `testBuilder` without a custom prelude.
-- 
-- We still need to specify a prelude file, because the user running these
-- tests might have installed a custom prelude.
test :: [String] -> String -> FilePath -> IO ()
test = testBuilder ("tests" </> "preludes") "default"

-- | A version of `testBuilder` using the preludes from "tests/preludes".
-- 
-- The first example from the README:
-- 
-- >>> test ["-d:", "-m"] "head" "passwd"
-- root
-- 
-- 
-- The second example, which adds `takeLast` to the user prelude:
-- 
-- >>> testPrelude "readme" ["-a"] "takeLast 3" "0-100"
-- 98
-- 99
-- 100
-- 
-- 
-- The last example, a quick test to validate that Hawk was properly installed:
-- 
-- >>> test [] "[1..3]" ""
-- 1
-- 2
-- 3
testPrelude :: FilePath -> [String] -> String -> FilePath -> IO ()
testPrelude = testBuilder ("tests" </> "preludes")

-- | A version of `testBuilder` using the preludes from the documentation.
-- 
-- All the examples from the documentation:
-- 
-- >>> test ["-a"] "L.reverse" "1-3"
-- 3
-- 2
-- 1
-- 
-- >>> test ["-ad"] "L.takeWhile (/=\"7\") . L.dropWhile (/=\"3\")" "1-10"
-- 3
-- 4
-- 5
-- 6
-- 
-- >>> testDoc "between" ["-ad"] "between \"3\" \"7\"" "1-10"
-- 3
-- 4
-- 5
-- 6
-- 
-- >>> test ["-a"] "L.take 3" "1-10"
-- 1
-- 2
-- 3
-- 
-- >>> test ["-m"] "L.reverse" "1-9"
-- 3 2 1
-- 6 5 4
-- 9 8 7
-- 
-- >>> testDoc "postorder" ["-ad"] "postorder (\\x -> printf \"(%s)\" . L.intercalate \" \" . (unpack x:))" "example.in"
-- (foo (bar1) (bar2 (baz)) (bar3))
-- 
-- >>> test ["-ad"] "sum . L.map (read . B.unpack)" "1-3"
-- 6
testDoc :: String -> [String] -> String -> FilePath -> IO ()
testDoc = testBuilder "doc"
