{-# LANGUAGE ExistentialQuantification
           , ExtendedDefaultRules
           , OverloadedStrings
           , ScopedTypeVariables #-}

module System.Console.Hawk.Representable (

    Row  (repr')
  , Rows (repr)
  , c8pack
  , sc8pack
  , listMap
  , listMapWords
  , printRows
  , printRow
  , parseRows
  , parseWords
  , showRows
  , runExpr
--  , runExprs

) where

import Prelude
import Control.Exception (SomeException,handle)
import qualified Data.ByteString.Char8 as SC8
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as C8 hiding (hPutStrLn)
import qualified Data.List as L
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.ByteString.Lazy.Search as BS
import Data.Map (Map)
import qualified Data.Map as M
import GHC.IO.Exception (IOErrorType(ResourceVanished),IOException(ioe_type))
import qualified System.IO as IO

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

parseRows :: SC8.ByteString -> C8.ByteString -> [C8.ByteString]
parseRows delim str = dropLastIfEmpty $ BS.split delim str

---- special case for space
parseWords :: SC8.ByteString -> SC8.ByteString -> C8.ByteString -> [[C8.ByteString]]
parseWords rowsDelim columnsDelim str = let rows = parseRows rowsDelim str
                                        in L.map f rows
    where f = if columnsDelim == SC8.singleton ' '
                then L.filter  (not . C8.null) . BS.split columnsDelim
                else BS.split columnsDelim
         
--parseWords :: SC8.ByteString -> [C8.ByteString] -> [[C8.ByteString]]
--parseWords delim strs = L.map f strs
--    where f = if delim == SC8.singleton ' '
--                then L.filter  (not . C8.null) . BS.split delim
--                else BS.split delim

--runOnInput :: Maybe FilePath -- ^ the input file or stdout when Nothing
--            -> (C8.ByteString -> IO ()) -- ^ the action to run on the input
--            -> IO ()
--runOnInput fp f = do
--    input <- maybe C8.getContents C8.readFile fp
--    f input
--    IO.hFlush IO.stdout
    -- TODO: we need also hFlush stderr?

runExpr :: Maybe FilePath -- ^ if the input is a file
        -> (Maybe FilePath -> IO C8.ByteString) -- ^ input reader
        -> (C8.ByteString -> C8.ByteString)
        -> (C8.ByteString -> IO ())
        -> IO ()
runExpr m i f o = i m >>= o . f
--runExpr = runOnInput
--
--runExprs :: Maybe FilePath -> (C8.ByteString -> [IO ()]) -> IO ()
--runExprs fp f = runOnInput fp (sequence_ . f)

-- ------------------------
-- Rows class and instances

-- | A type that instantiate Rows is a type that can be represented as
-- a list of rows, where typically a row is a line.
--
-- For example:
--
-- >>> mapM_ Data.ByteString.Lazy.Char8.putStrLn $ repr [1,2,3,4]
-- 1
-- 2
-- 3
-- 4
class (Show a) => Rows a where
    repr :: ByteString -- ^ column delimiter
         -> a -- ^ value to represent
         -> [C8.ByteString]
    repr _ = (:[]) . C8.pack . show

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
--printRows _ rd cd v = handle handler printRows_ 
--  where handler e = case ioe_type e of
--                      ResourceVanished -> return ()
--                      _ -> IO.hPrint IO.stderr e
--        printRows_ = C8.putStrLn (showRows rd cd v) >> IO.hFlush IO.stdout
--printRows b rowDelimiter columnDelimiter = printFirstRow_ . repr columnDelimiter
--    where printRows_ [] = return ()
--          printRows_ (x:xs) = do
--            putStrAndDelim x
--            handle ioExceptionsHandler (continue xs)
--          printFirstRow_ [] = return ()
--          printFirstRow_ (x:xs) = do
--            putStrOnly x
--            handle ioExceptionsHandler (continue xs)
--          putStrAndDelim = if b then handleErrors . putDelimAndStr_
--                                else putDelimAndStr_
--          putStrOnly = if b then handleErrors . C8.putStr else C8.putStr
--          putDelimAndStr_ :: C8.ByteString -> IO ()
--          putDelimAndStr_ c = C8.putStr rowDelimiter >> C8.putStr c
--          continue xs = IO.hFlush IO.stdout >> printRows_ xs
--          ioExceptionsHandler e = case ioe_type e of
--                                    ResourceVanished -> return ()
--                                    _ -> IO.hPrint IO.stderr e

instance Rows Bool
instance Rows Double
instance Rows Float
instance Rows Int
instance Rows Integer

instance Rows () where
    repr _ = const [C8.empty]

instance Rows Char where
    repr _ = (:[]) . C8.singleton

instance Rows ByteString where
    repr _ = (:[])

instance (Rows a) => Rows (Maybe a) where
    repr d = maybe [C8.empty] (repr d)

instance (Row a, Row b) => Rows (a,b) where
    repr d (x,y) = [repr' d x,repr' d y]

instance (Row a, Row b) => Rows (Map a b) where
    repr d = listAsRows d . M.toList

instance (ListAsRows a) => Rows (Set a) where
    repr d = listAsRows d . S.toList



-- Lists

class (Row a) => ListAsRows a where
    listAsRows :: ByteString -- ^ column delimiter
               -> [a]
               -> [ByteString]
    listAsRows d = L.map (repr' d)

instance ListAsRows ByteString
instance ListAsRows Bool
instance ListAsRows Double
instance ListAsRows Float
instance ListAsRows Int
instance ListAsRows Integer
instance (Row a) => ListAsRows (Maybe a)
instance ListAsRows ()
instance (ListAsRow a,ListAsRows a) => ListAsRows [a]
instance (Row a,Row b) => ListAsRows (a,b)
instance (Row a,Row b,Row c) => ListAsRows (a,b,c)

instance ListAsRows Char where
    listAsRows _ = (:[]) . C8.pack

instance (ListAsRows a) => Rows [a] where
    repr = listAsRows

instance (ListAsRow a,ListAsRows a) => ListAsRows (Set a) where
    listAsRows d = listAsRows d . L.map S.toList

instance (Row a,Row b) => ListAsRows (Map a b) where
    listAsRows d = listAsRows d . L.map M.toList

-- ---------------------------
-- Row class and instances

-- | A Row is something that can be expressed as a line. 
-- The output of repr' should be formatted such that
-- it can be read and processed from the command line.
--
-- For example:
--
-- >>> IO.putStrLn $ show [1,2,3,4]
-- [1,2,3,4]
--
-- >>> Data.ByteString.Lazy.Char8.putStrLn $ repr' [1,2,3,4]
-- 1 2 3 4
class (Show a) => Row a where
    repr' :: ByteString -- ^ delimiter
          -> a -- ^ value to represent
          -> ByteString
    repr' _ = C8.pack . show

instance Row Bool
instance Row Float
instance Row Double
instance Row Int
instance Row Integer
instance Row ()

instance Row Char where
    repr' _ = C8.singleton

printRow :: forall a . (Row a)
         => Bool -- ^ if printRow should continue after errors
         -> ByteString -- ^ the column delimiter
         -> a -- ^ the value to print
         -> IO ()
printRow b d = if b then handleErrors . f else f
  where f = C8.putStrLn . repr' d

class (Show a) => ListAsRow a where
    listRepr :: ByteString -> [a] -> ByteString
    listRepr d = C8.intercalate d . L.map (C8.pack . show)

instance ListAsRow Bool
instance ListAsRow Float
instance ListAsRow Int
instance ListAsRow Integer
instance ListAsRow ()

instance (ListAsRow a) => ListAsRow [a] where
    -- todo check the first delimiter if it should be d
    listRepr d = C8.intercalate d . L.map (listRepr d)

instance ListAsRow Char where
    listRepr _ = C8.pack

instance ListAsRow ByteString where
    listRepr d = C8.intercalate d

instance (Row a,Row b) => ListAsRow (a,b) where
    listRepr d = C8.intercalate d . 
                 L.map (\(x,y) -> C8.unwords [repr' d x,repr' d y])

instance (ListAsRow a) => Row [a] where
    repr' = listRepr

instance (ListAsRow a) => Row (Set a) where
    repr' d = listRepr d . S.toList

instance (Row a,Row b) => Row (Map a b) where
    repr' d = listRepr d . M.toList

instance Row ByteString where
    repr' _ = id

instance (Row a) => Row (Maybe a) where
    repr' _ Nothing = C8.empty
    repr' d (Just x) = repr' d x -- check if d is correct here

instance (Row a,Row b) => Row (a,b) where
    repr' d (a,b) = repr' d [repr' d a,repr' d b] 

instance (Row a,Row b,Row c) => Row (a,b,c) where
    repr' d (a,b,c) = repr' d [repr' d a,repr' d b,repr' d c]
