-- | In which --help prints the usage.
module System.Console.Hawk.Help
  ( help
  , failHelp
  ) where


import System.Environment
import System.Exit
import System.IO
import Text.Printf

import Control.Monad.Trans.OptionParser
import System.Console.Hawk.Args.Option


hPrintUsage :: Handle -> IO ()
hPrintUsage h = do
    hawk <- getProgName
    hPutStr h $ printf "Usage: %s [option]... <expr> [<file>]\n" hawk
    hPutStr h $ optionsHelp options


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
