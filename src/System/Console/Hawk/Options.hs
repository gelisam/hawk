module System.Console.Hawk.Options where

import Data.ByteString (ByteString)

import qualified Data.ByteString.Char8 as C8
import qualified Data.List as L
import qualified System.FilePath as FP

import System.Console.GetOpt


data Modes = EvalMode | StreamMode | LinesMode | MapMode | WordsMode
    deriving (Eq,Read,Show)

data Options = Options { optMode :: Modes 
                       , optLinesDelim :: ByteString
                       , optWordsDelim :: ByteString
                       , optRecompile :: Bool
                       , optHelp :: Bool
                       , optIgnoreErrors :: Bool
                       , optModuleFile :: Maybe FP.FilePath}
    deriving Show

defaultOptions :: Options
defaultOptions = Options { optMode = EvalMode
                         , optLinesDelim = C8.singleton '\n'
                         , optWordsDelim = C8.singleton ' '
                         , optRecompile = False
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
 -- delimiters
 [ Option ['D'] ["lines-delimiter"] (ReqArg delimiterAction "<delim>") delimiterHelp
 , Option ['d'] ["words-delimiter"] (ReqArg wordsDelimAction "<delim>") wordsDelimHelp

 -- modes
 , Option ['s'] ["stream"] (NoArg $ \o -> o{ optMode = StreamMode }) streamHelp
 , Option ['l'] ["lines"] (NoArg $ \o -> o{ optMode = LinesMode }) linesHelp
 , Option ['m'] ["map"] (NoArg $ \o -> o{ optMode = MapMode }) mapHelp
 , Option ['w'] ["words"] (NoArg $ \o -> o{ optMode = WordsMode }) wordsHelp

 -- other options
 , Option ['r'] ["recompile"] (NoArg setRecompile) recompileHelp
 , Option ['h'] ["help"] (NoArg $ \o -> o{ optHelp = True }) helpHelp
 , Option ['k'] ["keep-going"] (NoArg keepGoingAction) keepGoingHelp 
 ]
    where delimiterAction s o = let d = delimiter (C8.pack s)
                                in o{ optLinesDelim = d } 
          delimiterHelp = "lines delimiter, default '\\n'"
          wordsDelimAction s o = let d = delimiter (C8.pack s)
                                 in o{ optWordsDelim = d}
          wordsDelimHelp = "words delimiter, default ' '"
          setRecompile o = o{ optRecompile = True}
          recompileHelp = "recompile ~/.hawk/prelude.hs\neven if it did not change"
          
          streamHelp = "apply <expr> to the entire stream"
          linesHelp = "apply <expr> to the list of lines"
          mapHelp = "map <expr> over each line"
          wordsHelp = "map <expr> over the list of words of each line"

          helpHelp = "print this help message and exit"
          keepGoingAction o = o{ optIgnoreErrors = True}
          keepGoingHelp = "keep going when one line fails"

compileOpts :: [String] -> Either [String] (Options,[String])
compileOpts argv =
   case getOpt Permute options argv of
      (os,nos,[]) -> Right (L.foldl (.) id os defaultOptions, nos)
      (_,_,errs) -> Left errs

--postOptsProcessing :: String
--                   -> (Options,[String])
--                   -> Either [String] (Options,[String])
--postOptsProcessing defaultConfigFile (opts,args) =
--    if optHelp opts == False && length args < 1
--      then Left ["Error: Missing expression"]
--      else solveAmbiguities (opts,args) >>= Right . first process
--    where
----        errorArg = Left [
----                    "Missing argument representing the function to evaluate:\n"
----                    ++ "\t opts: " ++ show opts
----                    ++ "\n\targs: " ++ show args
----                    ]
--        process :: Options -> Options
--        process = optModuleFileProcess . optMapProcess
--        solveAmbiguities :: (Options,[String])
--                         -> Either [String] (Options,[String])
--        solveAmbiguities (os,as) =
--                    if optEval os && (optMap os || isJust (optDelimiter os))
--                      then Left ["Cannot set both -e and -m/-d options"]
--                      else Right (os,as)
--        optModuleFileProcess os = if isNothing (optModuleFile os)
--                                    then os{ optModuleFile = Just defaultConfigFile}
--                                    else os
--        optMapProcess os = if optMap os && isNothing (optDelimiter os)
--                              then os{ optDelimiter = Just (C8.singleton '\n')}
--                              else os
