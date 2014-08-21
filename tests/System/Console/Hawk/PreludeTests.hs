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

-- | A version of `test` without a custom input file either.
testEval :: [String] -> String -> IO ()
testEval flags expr = test flags expr ""


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
-- The last example from the README, a quick test to validate that Hawk was
-- properly installed:
-- 
-- >>> testEval [] "[1..3]"
-- 1
-- 2
-- 3
-- 
-- 
-- Making sure that we don't assume the user prelude exports "map":
-- 
-- >>> testPrelude "set" ["-m"] "const $ \"hello\"" "1-3"
-- hello
-- hello
-- hello
-- 
-- Making sure that we can find "map" even with NoImplicitPrelude:
-- 
-- >>> testPrelude "noImplicitPrelude" ["-m"] "\\_ -> hello" "1-3"
-- hello
-- hello
-- hello
-- 
-- Making sure sequences of whitespace count as one delimiter:
-- 
-- >>> testPrelude "default" ["-a"] "L.transpose" "1-12"
-- 1 4 7 10
-- 2 5 8 11
-- 3 6 9 12
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
-- 
-- >>> testDoc "conversions" ["-ad"] "sum . L.map toInt" "1-3"
-- 6
-- 
-- >>> testEval [] "2 ^ 100"
-- 1267650600228229401496703205376
-- 
-- >>> test ["-a"] "L.take 2" "1-9"
-- 1 2 3
-- 4 5 6
-- 
-- >>> test ["-m"] "L.take 2" "1-9"
-- 1 2
-- 4 5
-- 7 8
-- 
-- >>> test ["-a"] "show" "1-9"
-- [["1","2","3"],["4","5","6"],["7","8","9"]]
-- 
-- >>> test ["-a"] "id :: [[B.ByteString]] -> [[B.ByteString]]" "1-9"
-- 1 2 3
-- 4 5 6
-- 7 8 9
-- 
-- >>> test ["-a", "-d\\t"] "L.transpose" "1-9tabs"
-- 1	4	7
-- 2	5	8
-- 3	6	9
-- 
-- >>> test ["-ad,"] "L.transpose" "1-9commas"
-- 1,4,7
-- 2,5,8
-- 3,6,9
-- 
-- >>> test ["-D + ", "-d*", "-a"] "L.transpose" "equation"
-- x1*x2 + y1*y2 + z1*z2
-- 
-- >>> test ["-d", "-a"] "show :: [B.ByteString] -> String" "1-3"
-- ["1","2","3"]
-- 
-- >>> test ["-d", "-D", "-a"] "show :: B.ByteString -> String" "1-3"
-- "1\n2\n3\n"
-- 
-- >>> testEval [] "[[B.pack \"1\",B.pack \"2\"], [B.pack \"3\",B.pack \"4\"]]"
-- 1 2
-- 3 4
-- 
-- >>> testEval [] "[[\"1\",\"2\"], [\"3\",\"4\"]]"
-- 1 2
-- 3 4
-- 
-- >>> testEval [] "[[1,2], [3,4]] :: [[Float]]"
-- 1.0 2.0
-- 3.0 4.0
-- 
-- >>> testEval [] "1 :: Double"
-- 1.0
-- 
-- >>> testEval [] "(True,False)"
-- True
-- False
-- 
-- >>> testEval [] "[(1,2),(3,4)] :: [(Int,Float)]"
-- 1 2.0
-- 3 4.0
-- 
-- >>> testEval ["-O or "] "(True,False)"
-- True or False
-- 
-- >>> testEval ["-o\\t"] "[(1,2),(3,4.0)] :: [(Int,Float)]"
-- 1	2.0
-- 3	4.0
-- 
-- >>> test ["-m", "-d ", "-o*", "-D\\n", "-O+"] "id" "1-6"
-- 1*2*3+4*5*6
-- 
-- >> testEval ["-a"] "L.length"
-- 3
testDoc :: String -> [String] -> String -> FilePath -> IO ()
testDoc = testBuilder "doc"
