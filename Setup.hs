import           Control.Monad
import           Data.Maybe          (fromMaybe)
import           Distribution.Simple
import           System.Environment
--import System.IO


-- Surprisingly, if the user types "cabal install --enable-tests",
-- `args` will *not* be ["install", "--enable-tests"]. Instead, cabal will
-- run Setup.hs repeatedly with different arguments:
--
--     ["configure","--verbose=1","--builddir=dist/dist-sandbox-28d8356a","--ghc","--prefix=...",...]
--     ["build","--verbose=1","--builddir=dist/dist-sandbox-28d8356a"]
--     ["test","--builddir=dist/dist-sandbox-28d8356a"]
--     ["install","--verbose=1","--builddir=dist/dist-sandbox-28d8356a"]
--
-- We need to manipulate `args` via `substitute` in order to preserve those
-- extra arguments.
substitute :: Eq a => [(a, [a])] -> [a] -> [a]
substitute substitutions = (>>= go)
  where
    go x = fromMaybe (return x)
                     (lookup x substitutions)

main = do
    args <- getArgs

    --withFile "Setup.log" AppendMode $ \h -> do
    --  hPutStrLn h (show args)

    when ("test" `elem` args) $
      -- unlike most packages, this one needs to be installed before it can be tested.
      defaultMainArgs (substitute [ ("test", ["install","--verbose=1"])

                                  -- remove test-specific arguments
                                  , ("--log=$pkgid-$test-suite.log", [])
                                  , ("--machine-log=$pkgid.log", [])
                                  , ("--show-details=failures", [])
                                  ] args)

    defaultMainArgs args
