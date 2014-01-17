-- | In which --help prints the usage.
module System.Console.Hawk.Help
  ( help
  , failHelp
  ) where


import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

import System.Console.Hawk.Options


getUsage :: IO String
getUsage = do
    pn <- getProgName
    return $ usageInfo ("Usage: " ++ pn ++ " [<options>] <expr> [<file>]") 
                       options

hPrintUsage :: Handle -> IO ()
hPrintUsage h = do
    usage <- getUsage
    hPutStr h usage

help :: IO ()
help = hPrintUsage stdout

failHelp :: String -> IO ()
failHelp msg = do
    hPutStrLn stderr msg
    hPutStrLn stderr ""
    hPrintUsage stderr
    exitFailure
