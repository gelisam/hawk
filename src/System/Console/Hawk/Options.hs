module System.Console.Hawk.Options where

import Data.ByteString (ByteString)

import Control.Arrow (first)
import qualified Data.ByteString.Char8 as C8
import qualified Data.List as L
import qualified System.FilePath as FP

import System.Console.GetOpt
import Data.Maybe


data Options = Options { optDelimiter :: Maybe ByteString
                       , optRecompile :: Bool
                       , optMap :: Bool
                       , optEval :: Bool
                       , optHelp :: Bool
                       , optIgnoreErrors :: Bool
                       , optModuleFile :: Maybe FP.FilePath}
    deriving Show

defaultOptions :: Options
defaultOptions = Options { optDelimiter = Nothing
                         , optRecompile = False
                         , optMap = False
                         , optEval = False
                         , optHelp = False
                         , optIgnoreErrors = False
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
 [ Option ['d'] ["delimiter"] (OptArg delimiterAction "<delim>") delimiterHelp
 , Option ['r'] ["recompile"] (NoArg setRecompile) recompileHelp
 , Option ['m'] ["map"] (NoArg $ \o -> o{ optMap = True}) mapHelp
 , Option ['e'] ["eval"] (NoArg $ \o -> o{ optEval = True}) evalHelp
 , Option ['h'] ["help"] (NoArg $ \o -> o{ optHelp = True }) helpHelp
 , Option ['k'] ["keep-going"] (NoArg keepGoingAction) keepGoingHelp 
 ]
    where delimiterAction s o = let d = case s of
                                         Nothing -> C8.singleton '\n'
                                         Just rd -> delimiter (C8.pack rd)
                                in o{ optDelimiter = Just d } 
          delimiterHelp = "line-delimiter, defaults to '\\n'"
          setRecompile o = o{ optRecompile = True}
          recompileHelp = "recompile ~/.hawk/prelude.hs\neven if it did not change"
          mapHelp = "map <expr> over each line"
          evalHelp = "evaluate the value of <expr>"
          helpHelp = "print this help message and exit"
          keepGoingAction o = o{ optIgnoreErrors = True}
          keepGoingHelp = "keep going when one line fails"

compileOpts :: [String] -> Either [String] (Options,[String])
compileOpts argv =
   case getOpt Permute options argv of
      (os,nos,[]) -> Right (L.foldl (.) id os defaultOptions, nos)
      (_,_,errs) -> Left errs

postOptsProcessing :: String
                   -> (Options,[String])
                   -> Either [String] (Options,[String])
postOptsProcessing defaultConfigFile (opts,args) =
    if optHelp opts == False && length args < 1
      then Left ["Error: Missing expression"]
      else solveAmbiguities (opts,args) >>= Right . first process
    where
--        errorArg = Left [
--                    "Missing argument representing the function to evaluate:\n"
--                    ++ "\t opts: " ++ show opts
--                    ++ "\n\targs: " ++ show args
--                    ]
        process :: Options -> Options
        process = optModuleFileProcess . optMapProcess
        solveAmbiguities :: (Options,[String])
                         -> Either [String] (Options,[String])
        solveAmbiguities (os,as) =
                    if optEval os && (optMap os || isJust (optDelimiter os))
                      then Left ["Cannot set both -e and -m/-d options"]
                      else Right (os,as)
        optModuleFileProcess os = if isNothing (optModuleFile os)
                                    then os{ optModuleFile = Just defaultConfigFile}
                                    else os
        optMapProcess os = if optMap os && isNothing (optDelimiter os)
                              then os{ optDelimiter = Just (C8.singleton '\n')}
                              else os
