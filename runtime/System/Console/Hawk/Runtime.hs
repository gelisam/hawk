{-# LANGUAGE OverloadedStrings #-}
-- | Applying the user expression as directed by the HawkRuntime.
--   The API may change at any time.
module System.Console.Hawk.Runtime
  ( processTable
  ) where

import Control.Applicative
import Control.Exception
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TextIO
import GHC.IO.Exception
import System.IO

import System.Console.Hawk.Args.Spec
import System.Console.Hawk.Representable
import System.Console.Hawk.Runtime.Base


processTable :: Rows a => HawkRuntime -> ([[T.Text]] -> a) -> HawkIO ()
processTable runtime f = HawkIO $ do
    xss <- getTable (inputSpec runtime)
    outputRows (outputSpec runtime) (f xss)


getTable :: InputSpec -> IO [[T.Text]]
getTable spec = splitIntoTable' <$> getInputString'
  where
    splitIntoTable' = splitIntoTable (inputFormat spec)
    getInputString' = getInputString (inputSource spec)

getInputString :: InputSource -> IO T.Text
getInputString NoInput = return ""
getInputString UseStdin = TextIO.getContents
getInputString (InputFile f) = TextIO.readFile f

-- [[contents]]
-- or
-- [[record0], [record1], ...]
-- or
-- [[field0, field1, ...], [field0, field1, ...], ...]
splitIntoTable :: InputFormat -> T.Text -> [[T.Text]]
splitIntoTable RawStream = return . return
splitIntoTable (Records sep format) = fmap splitIntoFields' . splitIntoRecords'
  where
    splitIntoFields' = splitIntoFields format
    splitIntoRecords' = splitAtSeparator sep

-- [record]
-- or
-- [field0, field1, ...]
splitIntoFields :: RecordFormat -> T.Text -> [T.Text]
splitIntoFields RawRecord = return
splitIntoFields (Fields sep) = splitAtSeparator sep

splitAtSeparator :: Separator -> T.Text -> [T.Text]
splitAtSeparator Whitespace = T.words
splitAtSeparator (Delimiter "\n") = fmap dropWindowsNewline . T.lines
  where
    dropWindowsNewline :: T.Text -> T.Text
    dropWindowsNewline "" = ""
    dropWindowsNewline s
        | last_char == '\r' = s'
        | otherwise = s
      where
        last_char = T.last s
        n = T.length s
        s' = T.take (n - 1) s
splitAtSeparator (Delimiter d) = T.splitOn d


outputRows :: Rows a => OutputSpec -> a -> IO ()
outputRows (OutputSpec _ spec) x = ignoringBrokenPipe $ do
    let s = join' (toRows x)
    TextIO.putStr s
    hFlush stdout
  where
    join' = join (recordDelimiter spec)
    toRows = repr (fieldDelimiter spec)

    join :: T.Text -> [T.Text] -> T.Text
    join "\n" = T.unlines
    join sep  = T.intercalate sep

-- Don't fret if stdout is closed early, that is the way of shell pipelines.
ignoringBrokenPipe :: IO () -> IO ()
ignoringBrokenPipe = handleJust isBrokenPipe $ \_ -> do
    -- ignore the broken pipe
    return ()
  where
    isBrokenPipe e | ioe_type e == ResourceVanished = Just e
    isBrokenPipe _ | otherwise                      = Nothing
