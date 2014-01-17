-- | In which --help prints the usage.
module System.Console.Hawk.Help
  ( help
  , failHelp
  ) where


import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Printf

import System.Console.Hawk.Options


hPrintUsage :: Handle -> IO ()
hPrintUsage h = do
    hawk <- getProgName
    let header = printf "Usage: %s [options] <expr> [<file>]" hawk
    let usage = usageInfo header options
    hPutStr h usage


help :: IO ()
help = hPrintUsage stdout

-- | A version of `help` which prints the usage to stderr, after printing a
--   custom error message.
failHelp :: String -> IO ()
failHelp msg = do
    hPutStrLn stderr msg
    hPutStrLn stderr ""
    hPrintUsage stderr
    exitFailure
