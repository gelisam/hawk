{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
-- | The part of a HawkSpec used at Runtime. The API may change at any time.
module System.Console.Hawk.Runtime.Base
  ( HawkRuntime(..)
  , processTable
  ) where

import Control.Applicative
import Control.Exception
import Data.ByteString.Lazy.Char8 as B
import Data.ByteString.Lazy.Search as Search
import Data.Typeable.Internal
import GHC.IO.Exception
import System.IO

import System.Console.Hawk.Args.Spec
import System.Console.Hawk.Representable


data HawkRuntime = HawkRuntime
    { inputSpec :: InputSpec
    , outputSpec :: OutputSpec
    }
  deriving (Show, Eq, Typeable)

processTable :: Rows a => HawkRuntime -> ([[B.ByteString]] -> a) -> IO ()
processTable runtime f = do
    xss <- getTable (inputSpec runtime)
    outputRows (outputSpec runtime) (f xss)


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
-- [[line0], [line1], ...]
-- or
-- [[field0, field1, ...], [field0, field1, ...], ...]
splitIntoTable :: InputFormat -> B.ByteString -> [[B.ByteString]]
splitIntoTable RawStream = return . return
splitIntoTable (Lines sep format) = fmap splitIntoFields' . splitIntoLines'
  where
    splitIntoFields' = splitIntoFields format
    splitIntoLines' = splitIntoLines sep

-- [line0, line1, ...]
splitIntoLines :: Separator -> B.ByteString -> [B.ByteString]
splitIntoLines "\n" = fmap dropWindowsNewline . B.lines
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
splitIntoLines sep = Search.split sep

-- [line]
-- or
-- [field0, field1, ...]
splitIntoFields :: LineFormat -> B.ByteString -> [B.ByteString]
splitIntoFields RawLine = return
splitIntoFields (Words sep) = Search.split sep


outputRows :: Rows a => OutputSpec -> a -> IO ()
outputRows (OutputSpec _ spec) x = ignoringBrokenPipe $ do
    let s = join' (toRows x)
    B.putStr s
    hFlush stdout
  where
    join' = join (B.fromStrict $ lineDelimiter spec)
    toRows = repr (B.fromStrict $ wordDelimiter spec)
    
    join :: B.ByteString -> [B.ByteString] -> B.ByteString
    join "\n" = B.unlines
    join sep  = B.intercalate sep

-- Don't fret if stdout is closed early, that is the way of shell pipelines.
ignoringBrokenPipe :: IO () -> IO ()
ignoringBrokenPipe = handleJust isBrokenPipe $ \_ -> do
    -- ignore the broken pipe
    return ()
  where
    isBrokenPipe e | ioe_type e == ResourceVanished = Just e
    isBrokenPipe _ | otherwise                      = Nothing
