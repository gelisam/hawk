{-# LANGUAGE ExistentialQuantification, OverloadedStrings, RankNTypes #-}
-- | Applying the user expression as directed by the HawkRuntime.
--   The API may change at any time.
module System.Console.Hawk.Runtime
  ( SomeRows(..)
  , processTable
  ) where

import Control.Applicative
import Control.Exception
import Data.ByteString.Lazy.Char8 as B
import Data.ByteString.Lazy.Search as Search
import GHC.IO.Exception
import System.IO

import System.Console.Hawk.Args.Spec
import System.Console.Hawk.Representable
import System.Console.Hawk.Runtime.Base


data SomeRows = forall a. Rows a => SomeRows a

processTable :: HawkRuntime -> ([[B.ByteString]] -> SomeRows) -> HawkIO ()
processTable runtime f = HawkIO $ do
    xss <- getTable (inputSpec runtime)
    case f xss of
      SomeRows x -> outputRows (outputSpec runtime) x


getTable :: InputSpec -> IO [[B.ByteString]]
getTable spec = splitIntoTable' <$> getInputString'
  where
    splitIntoTable' = splitIntoTable (inputFormat spec)
    getInputString' = getInputString (inputSource spec)

getInputString :: InputSource -> IO B.ByteString
getInputString NoInput = return B.empty
getInputString UseStdin = B.getContents
getInputString (InputFile f) = B.readFile f

-- [[contents]]
-- or
-- [[record0], [record1], ...]
-- or
-- [[field0, field1, ...], [field0, field1, ...], ...]
splitIntoTable :: InputFormat -> B.ByteString -> [[B.ByteString]]
splitIntoTable RawStream = return . return
splitIntoTable (Records sep format) = fmap splitIntoFields' . splitIntoRecords'
  where
    splitIntoFields' = splitIntoFields format
    splitIntoRecords' = splitAtSeparator sep

-- [record]
-- or
-- [field0, field1, ...]
splitIntoFields :: RecordFormat -> B.ByteString -> [B.ByteString]
splitIntoFields RawRecord = return
splitIntoFields (Fields sep) = splitAtSeparator sep

splitAtSeparator :: Separator -> B.ByteString -> [B.ByteString]
splitAtSeparator Whitespace = B.words
splitAtSeparator (Delimiter "\n") = fmap dropWindowsNewline . B.lines
  where
    dropWindowsNewline :: B.ByteString -> B.ByteString
    dropWindowsNewline "" = ""
    dropWindowsNewline s
        | last_char == '\r' = s'
        | otherwise = s
      where
        last_char = B.last s
        n = B.length s
        s' = B.take (n - 1) s
splitAtSeparator (Delimiter d) = Search.split d


outputRows :: Rows a => OutputSpec -> a -> IO ()
outputRows (OutputSpec out spec) x = ignoringBrokenPipe $ do
    let s = join' (toRows x)
    case out of
        UseStdout                             -> B.putStr s >> hFlush stdout
        OutputFile (FileSink f Nothing)       -> B.writeFile f s
        OutputFile (FileSink f (Just backup)) -> do
            copyFile f backup
            B.writeFile f s
  where
    join' = join (B.fromStrict $ recordDelimiter spec)
    toRows = repr (B.fromStrict $ fieldDelimiter spec)

    join :: B.ByteString -> [B.ByteString] -> B.ByteString
    join "\n" = B.unlines
    join sep  = B.intercalate sep
    copyFile i o = B.readFile i >>= \f -> B.writeFile o f

-- Don't fret if stdout is closed early, that is the way of shell pipelines.
ignoringBrokenPipe :: IO () -> IO ()
ignoringBrokenPipe = handleJust isBrokenPipe $ \_ ->
    -- ignore the broken pipe
    return ()
  where
    isBrokenPipe e | ioe_type e == ResourceVanished = Just e
    isBrokenPipe _                                  = Nothing
