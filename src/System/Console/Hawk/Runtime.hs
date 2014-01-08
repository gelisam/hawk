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

{-# LANGUAGE ExistentialQuantification
           , ExtendedDefaultRules
           , OverloadedStrings
           , ScopedTypeVariables #-}

-- | This is Hawk's runtime, it needs to be installed in order to evaluate Hawk
--   expressions. The API may change at any time.
module System.Console.Hawk.Runtime (
    c8pack
  , sc8pack
  , listMap
  , listMapWords
  , printRows
  , printRow
  , parseRows
  , parseWords
  , showRows
  , runExpr
) where

import Prelude
import Control.Exception (SomeException,handle)
import qualified Data.ByteString.Char8 as SC8
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as C8 hiding (hPutStrLn)
import qualified Data.List as L
import qualified Data.ByteString.Lazy.Search as BS
import qualified System.IO as IO

import System.Console.Hawk.Representable
import qualified System.Console.Hawk.IO as HawkIO

handleErrors :: IO () -> IO ()
handleErrors = handle (\(e :: SomeException) -> IO.hPrint IO.stderr e)

dropLastIfEmpty :: [C8.ByteString] -> [C8.ByteString]
dropLastIfEmpty [] = []
dropLastIfEmpty (x:[]) = if C8.null x then [] else [x]
dropLastIfEmpty (x:xs) = x:dropLastIfEmpty xs

listMap :: (a -> b) -> [a] -> [b]
listMap = L.map

listMapWords :: ([a] -> b) -> [[a]] -> [b]
listMapWords = L.map

c8pack :: String
       -> C8.ByteString
c8pack = C8.pack

sc8pack :: String
        -> SC8.ByteString
sc8pack = SC8.pack

dropTrailingNewline :: C8.ByteString -> C8.ByteString
dropTrailingNewline "" = ""
dropTrailingNewline s 
    | last_char == '\r' = s'
    | otherwise = s
  where
    last_char = C8.last s
    n = C8.length s
    s' = C8.take (n - 1) s

-- if delim is "\n", Windows-style "\r\n" delimiters are also accepted.
parseRows :: SC8.ByteString -> C8.ByteString -> [C8.ByteString]
parseRows delim = dropLastIfEmpty . maybeDropTrailingNewline . BS.split delim
    where maybeDropTrailingNewline
              | delim == "\n" = map dropTrailingNewline
              | otherwise = id

---- special case for space
parseWords :: SC8.ByteString -> SC8.ByteString -> C8.ByteString -> [[C8.ByteString]]
parseWords rowsDelim columnsDelim str = L.map f rows
    where 
        f 
            | columnsDelim == SC8.singleton ' ' = L.filter (not . C8.null) . BS.split columnsDelim
            | otherwise = BS.split columnsDelim
        rows = parseRows rowsDelim str

runExpr :: Maybe FilePath -- ^ if the input is a file
        -> (Maybe FilePath -> IO C8.ByteString) -- ^ input reader
        -> (C8.ByteString -> C8.ByteString)
        -> (C8.ByteString -> IO ())
        -> IO ()
runExpr m i f o = i m >>= o . f

showRows :: forall a . (Rows a)
         => C8.ByteString -- ^ rows delimiter
         -> C8.ByteString -- ^ columns delimiter
         -> a -- ^ value to print
         -> C8.ByteString
showRows rd cd = C8.intercalate rd . repr cd

printRows :: forall a . (Rows a) 
          => Bool -- ^ if printRows will continue after errors
          -> C8.ByteString -- ^ rows delimiter
          -> C8.ByteString -- ^ columns delimiter
          -> a -- ^ the value to print as rows
          -> IO ()
printRows _ rd cd = HawkIO.printOutput . showRows rd cd

printRow :: forall a . (Row a)
         => Bool -- ^ if printRow should continue after errors
         -> ByteString -- ^ the column delimiter
         -> a -- ^ the value to print
         -> IO ()
printRow b d = if b then handleErrors . f else f
  where f = C8.putStrLn . repr' d

