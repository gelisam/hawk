{-# LANGUAGE ExistentialQuantification, OverloadedStrings, RankNTypes #-}
-- | Applying the user expression as directed by the HawkRuntime.
--   The API may change at any time.
module System.Console.Hawk.Runtime
  ( SomeRows(..)
  , processTable
  ) where

import Control.Exception
import qualified Data.List as L
import Data.List.Split
import GHC.IO.Exception
import System.IO

import System.Console.Hawk.Args.Spec
import System.Console.Hawk.Representable
import System.Console.Hawk.Runtime.Base


data SomeRows = forall a. Rows a => SomeRows a

processTable :: HawkRuntime -> ([[String]] -> SomeRows) -> HawkIO ()
processTable runtime f = HawkIO $ do
    xss <- getTable (inputSpec runtime)
    case f xss of
      SomeRows x -> outputRows (outputSpec runtime) x


getTable :: InputSpec -> IO [[String]]
getTable spec = splitIntoTable' <$> getInputString'
  where
    splitIntoTable' = splitIntoTable (inputFormat spec)
    getInputString' = getInputString (inputSource spec)

getInputString :: InputSource -> IO String
getInputString NoInput = return mempty
getInputString UseStdin = getContents
getInputString (InputFile f) = readFile f

-- [[contents]]
-- or
-- [[record0], [record1], ...]
-- or
-- [[field0, field1, ...], [field0, field1, ...], ...]
splitIntoTable :: InputFormat -> String -> [[String]]
splitIntoTable RawStream = return . return
splitIntoTable (Records sep format) = fmap splitIntoFields' . splitIntoRecords'
  where
    splitIntoFields' = splitIntoFields format
    splitIntoRecords' = splitAtSeparator sep

-- [record]
-- or
-- [field0, field1, ...]
splitIntoFields :: RecordFormat -> String -> [String]
splitIntoFields RawRecord = return
splitIntoFields (Fields sep) = splitAtSeparator sep

splitAtSeparator :: Separator -> String -> [String]
splitAtSeparator Whitespace = words
splitAtSeparator (Delimiter "\n") = fmap dropWindowsNewline . lines
  where
    dropWindowsNewline :: String -> String
    dropWindowsNewline "" = ""
    dropWindowsNewline s
        | last_char == '\r' = s'
        | otherwise = s
      where
        last_char = last s
        n = length s
        s' = take (n - 1) s
splitAtSeparator (Delimiter d) = splitOn d


outputRows :: Rows a => OutputSpec -> a -> IO ()
outputRows (OutputSpec _ spec) x = ignoringBrokenPipe $ do
    let s = join' (toRows x)
    putStr s
    hFlush stdout
  where
    join' = join (recordDelimiter spec)
    toRows = repr (fieldDelimiter spec)

    join :: String -> [String] -> String
    join "\n" = unlines
    join sep  = L.intercalate sep

-- Don't fret if stdout is closed early, that is the way of shell pipelines.
ignoringBrokenPipe :: IO () -> IO ()
ignoringBrokenPipe = handleJust isBrokenPipe $ \_ ->
    -- ignore the broken pipe
    return ()
  where
    isBrokenPipe e | ioe_type e == ResourceVanished = Just e
    isBrokenPipe _                                  = Nothing
