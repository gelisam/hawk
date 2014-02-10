--   Copyright 2013 Mario Pastorelli (pastorelli.mario@gmail.com) Samuel Gélineau (gelisam@gmail.com)
--
--   Licensed under the Apache License, Version 2.0 (the "License");
--   you may not use this file except in compliance with the License.
--   You may obtain a copy of the License at
--
--       http://www.apache.org/licenses/LICENSE-2.0
--
--   Unless required by applicable law or agreed to in writing, software
--   distributed under the License is distributed on an "AS IS" BASIS,
--   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--   See the License for the specific language governing permissions and
--   limitations under the License.

-- | In which the flags accepted by the Hawk executable are sorted out.
module System.Console.Hawk.Options where

import Data.ByteString (ByteString)

import qualified Data.ByteString.Char8 as C8
import qualified Data.List as L
import qualified System.FilePath as FP

import System.Console.GetOpt


data Mode = EvalMode | ApplyMode | MapMode
    deriving (Eq,Enum,Read,Show)

data Options = Options { optMode :: Mode
                       , optLinesDelim :: Maybe ByteString
                       , optWordsDelim :: Maybe ByteString
                       , optOutLinesDelim :: Maybe ByteString
                       , optOutWordsDelim :: Maybe ByteString
                       , optRecompile :: Bool
                       , optVersion :: Bool
                       , optHelp :: Bool}
    deriving Show

defaultOptions :: Options
defaultOptions = Options { optMode = EvalMode
                         , optLinesDelim = Nothing -- C8.singleton '\n'
                         , optWordsDelim = Nothing -- C8.singleton ' '
                         , optOutLinesDelim = Nothing
                         , optOutWordsDelim =  Nothing
                         , optRecompile = False
                         , optVersion = False
                         , optHelp = False}

-- | Handle a few typical but exceptional delimiters.
-- 
-- >>> (C8.unpack . delimiter . C8.pack) ","
-- ","
-- 
-- >>> (C8.unpack . delimiter . C8.pack) "\\n"
-- "\n"
-- 
-- >>> (C8.unpack . delimiter . C8.pack) "\\t"
-- "\t"
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
 [ Option ['D'] ["lines-delimiter"] (OptArg delimiterAction "<delim>") delimiterHelp
 , Option ['d'] ["words-delimiter"] (OptArg wordsDelimAction "<delim>") wordsDelimHelp
 , Option ['O'] ["output-lines-delim"] (OptArg outDelimAction "<delim>") outDelimHelp
 , Option ['o'] ["output-words-delim"] (OptArg outWordsDelimAction "<delim>") outWordsDelimHelp

 -- modes
 , Option ['a'] ["apply"] (NoArg $ setMode ApplyMode) applyHelp
 , Option ['m'] ["map"] (NoArg $ setMode MapMode) mapHelp

 -- other options
 , Option ['r'] ["recompile"] (NoArg setRecompile) recompileHelp
 , Option ['v'] ["version"] (NoArg $ \o -> o{ optVersion = True }) versionHelp
 , Option ['h'] ["help"] (NoArg $ \o -> o{ optHelp = True }) helpHelp
 ]
    where outDelimAction d o = o{ optOutLinesDelim = fmap (delimiter . C8.pack) d }
          outDelimHelp = "output lines delimiter, default " ++
                         "is equal to the input lines delimiter (-D)"
          outWordsDelimAction d o = o{ optOutWordsDelim = fmap (delimiter . C8.pack) d }
          outWordsDelimHelp = "output words delimiter, default " ++
                              "is equal to the input words delimiter (-d)"
          makeDelim ms = case ms of
                           Nothing -> C8.pack ""
                           Just "" -> C8.pack ""
                           Just s -> delimiter $ C8.pack s
          delimiterAction ms o = o{ optLinesDelim = Just (makeDelim ms) }
          delimiterHelp = "lines delimiter, default '\\n'"
          wordsDelimAction ms o = o{ optWordsDelim = Just (makeDelim ms) }
          wordsDelimHelp = "words delimiter, default ' '"
          setRecompile o = o{ optRecompile = True}
          recompileHelp = "recompile ~/.hawk/prelude.hs\neven if it did not change"
          
          applyHelp = "apply <expr> to the stream"
          mapHelp = "map <expr> to the stream"

          versionHelp = "print the version number and exit"
          helpHelp = "print this help message and exit"
          setMode m o = o{ optMode = m }

-- getOpt parses the option in the order they appear, but if we want to keep
-- the last copy of a given flag, the resulting functions need to be composed
-- in reverse order.
compileOpts :: [String] -> Either [String] (Options,[String])
compileOpts argv =
   case getOpt Permute options argv of
      (os,nos,[]) -> Right (L.foldl (.) id (L.reverse os) defaultOptions, nos)
      (_,_,errs) -> Left errs
