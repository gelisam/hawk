module HSProcess.Options where

import Data.ByteString (ByteString)

import qualified Data.ByteString.Char8 as C8
import qualified Data.List as L
import qualified System.FilePath as FP

import System.Console.GetOpt
import Data.Maybe


data Options = Options { optDelimiter :: Maybe ByteString
                       , optRecompile :: Bool
                       , optMap :: Bool
                       , optHelp :: Bool
                       , optIgnoreErrors :: Bool
                       , optModuleFile :: Maybe FP.FilePath}
    deriving Show

defaultOptions :: Options
defaultOptions = Options { optDelimiter = Nothing
                         , optRecompile = False
                         , optMap = False
                         , optHelp = False
                         , optIgnoreErrors = True
                         , optModuleFile = Nothing }

delimiter :: ByteString -> ByteString
delimiter = C8.concat . (\ls -> L.head ls:L.map subFirst (L.tail ls))
                     . C8.splitWith (== '\\')
    where subFirst s = case C8.head s of
                        'n' -> C8.cons '\n' $ C8.tail s
                        't' -> C8.cons '\t' $ C8.tail s
                        _ -> s

options :: [OptDescr (Options -> Options)]
options = 
 [ Option ['d'] ["delimiter"] (OptArg delimiterAction "<String>") delimiterHelp
 , Option ['r'] ["recompile"] (NoArg setRecompile) recompileHelp
 , Option ['m'] ["map"] (NoArg $ \o -> o{ optMap = True}) mapHelp
 , Option ['h'] ["help"] (NoArg $ \o -> o{ optHelp = True }) helpHelp
 , Option ['e'] ["errors"] (NoArg ignoreErrorsAction) ignoreErrorsHelp 
 ]
    where delimiterAction s o = let d = case s of
                                         Nothing -> C8.singleton '\n'
                                         Just rd -> delimiter (C8.pack rd)
                                in o{ optDelimiter = Just d } 
          delimiterHelp = "String used as delimiter"
          setRecompile o = o{ optRecompile = True}
          recompileHelp = "Recompile toolkit.hs"
          mapHelp = "Map a command over each string separated by the delimiter"
          helpHelp = "Print help and exit"
          ignoreErrorsAction o = o{ optIgnoreErrors = False}
          ignoreErrorsHelp = "When set, errors in user code block "
                          ++ "execution. When is not set,"
                          ++ " errors don't block the execution and "
                          ++ "are logged to stderr. Default: False"

compileOpts :: [String] -> Either [String] (Options,[String])
compileOpts argv =
   case getOpt Permute options argv of
      (os,nos,[]) -> Right (L.foldl (.) id os $ defaultOptions, nos)
      (_,_,errs) -> Left errs

postOptsProcessing :: String 
                   -> (Options,[String])
                   -> Either [String] (Options,[String])
postOptsProcessing defaultConfigFile (opts,args) = if length args < 1
                                                    then errorArg
                                                    else Right (process opts,args)
    where
        errorArg = Left $ [
                    "Missing argument representing the function to evaluate:\n"
                    ++ "\t opts: " ++ show opts
                    ++ "\n\targs: " ++ show args
                    ]
        process = optModuleFileProcess . optMapProcess
        optModuleFileProcess os = if isNothing (optModuleFile os)
                                    then os{ optModuleFile = Just defaultConfigFile}
                                    else os
        optMapProcess os = if optMap os && isNothing (optDelimiter os)
                              then os{ optDelimiter = Just (C8.singleton '\n')}
                              else os
