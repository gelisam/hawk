{-# LANGUAGE ExistentialQuantification
           , ExtendedDefaultRules
           , OverloadedStrings
           , ScopedTypeVariables #-}

module System.Console.Hawk.Representable (

    Row  (repr')
  , Rows (repr)
  , listMap
  , printRows
  , printRow
  , parseRows
  , runExpr
  , runExprs

) where

import Prelude
import Control.Exception (SomeException,handle)
import qualified Data.ByteString as StrictBS
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

handleErrors :: IO () -> IO ()
handleErrors = handle (\(e :: SomeException) -> IO.hPrint IO.stderr e)

dropLastIfEmpty :: [C8.ByteString] -> [C8.ByteString]
dropLastIfEmpty [] = []
dropLastIfEmpty (x:[]) = if C8.null x then [] else [x]
dropLastIfEmpty (x:xs) = x:dropLastIfEmpty xs

listMap :: (a -> b) -> [a] -> [b]
listMap = L.map

parseRows :: StrictBS.ByteString -> C8.ByteString -> [C8.ByteString]
parseRows delim str = dropLastIfEmpty $ BS.split delim str

runExpr :: Maybe FilePath -> (C8.ByteString -> IO ()) -> IO ()
runExpr fp f = do
    input <- maybe C8.getContents C8.readFile fp
    f input

runExprs :: Maybe FilePath -> (C8.ByteString -> [IO ()]) -> IO ()
runExprs fp f = do
    input <- maybe C8.getContents C8.readFile fp
    sequence_ (f input)

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
    repr :: a -> [C8.ByteString]
    repr = (:[]) . C8.pack . show

printRows :: forall a . (Rows a) => Bool -> a -> IO ()
printRows b = printRows_ . repr
    where printRows_ [] = return ()
          printRows_ (x:xs) = do
            f x
            handle ioExceptionsHandler (continue xs)
          f = if b then handleErrors . C8.putStrLn else C8.putStrLn
          continue xs = IO.hFlush IO.stdout >> printRows_ xs
          ioExceptionsHandler e = case ioe_type e of
                                    ResourceVanished -> return ()
                                    _ -> IO.hPrint IO.stderr e

instance Rows Bool
instance Rows Double
instance Rows Float
instance Rows Int
instance Rows Integer

instance Rows () where
    repr = const [C8.empty]

instance Rows Char where
    repr = (:[]) . C8.singleton

instance Rows ByteString where
    repr = (:[])

instance (Rows a) => Rows (Maybe a) where
    repr = maybe [C8.empty] repr

instance (Row a, Row b) => Rows (a,b) where
    repr (x,y) = [repr' x,repr' y]

instance (Row a, Row b) => Rows (Map a b) where
    repr = listAsRows . M.toList

instance (ListAsRows a) => Rows (Set a) where
    repr = listAsRows . S.toList



-- Lists

class (Row a) => ListAsRows a where
    listAsRows :: [a] -> [ByteString]
    listAsRows = L.map repr'

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
    listAsRows = (:[]) . C8.pack

instance (ListAsRows a) => Rows [a] where
    repr = listAsRows

instance (ListAsRow a,ListAsRows a) => ListAsRows (Set a) where
    listAsRows = listAsRows . L.map S.toList

instance (Row a,Row b) => ListAsRows (Map a b) where
    listAsRows = listAsRows . L.map M.toList

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
    repr' :: a -> ByteString
    repr' = C8.pack . show

instance Row Bool
instance Row Float
instance Row Double
instance Row Int
instance Row Integer
instance Row ()

instance Row Char where
    repr' = C8.singleton

printRow :: forall a . (Row a) => Bool -> a -> IO ()
printRow b = if b then handleErrors . f else f
  where f = C8.putStrLn . repr'

class (Show a) => ListAsRow a where
    listRepr :: [a] -> ByteString
    listRepr = C8.intercalate "\t" . L.map (C8.pack . show)

instance ListAsRow Bool
instance ListAsRow Float
instance ListAsRow Int
instance ListAsRow Integer
instance ListAsRow ()

instance (ListAsRow a) => ListAsRow [a] where
    listRepr = C8.intercalate "\t" . L.map listRepr

instance ListAsRow Char where
    listRepr = C8.pack

instance ListAsRow ByteString where
    listRepr = C8.intercalate "\t"

instance (Row a,Row b) => ListAsRow (a,b) where
    listRepr = C8.intercalate "\t" . L.map (\(x,y) -> C8.unwords [repr' x,repr' y])

instance (ListAsRow a) => Row [a] where
    repr' = listRepr

instance (ListAsRow a) => Row (Set a) where
    repr' = listRepr . S.toList

instance (Row a,Row b) => Row (Map a b) where
    repr' = listRepr . M.toList

instance Row ByteString where
    repr' = id

instance (Row a) => Row (Maybe a) where
    repr' Nothing = C8.empty
    repr' (Just x) = repr' x

instance (Row a,Row b) => Row (a,b) where
    repr' (a,b) = repr' [repr' a,repr' b] 

instance (Row a,Row b,Row c) => Row (a,b,c) where
    repr' (a,b,c) = repr' [repr' a,repr' b,repr' c]
