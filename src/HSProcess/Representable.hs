{-# LANGUAGE DeriveDataTypeable
           , ExistentialQuantification
           , ExtendedDefaultRules
           , OverloadedStrings
           , ScopedTypeVariables #-}

module HSProcess.Representable (
    Row
  , Rows
  , printRows
  , printRow

) where

import Control.Exception (SomeException,handle)
import Data.ByteString.Lazy.Char8
import qualified Data.ByteString.Lazy.Char8 as C8 hiding (hPutStrLn)
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Typeable (Typeable)
import qualified System.IO as IO

handleErrors :: IO () -> IO ()
handleErrors = handle (\(e :: SomeException) -> IO.hPutStrLn IO.stderr (show e))


-- ------------------------
-- Rows class and instances

-- | A type that instantiate Rows is a type that can be represented as
-- a list of string, one per row.
class (Typeable a, Show a) => Rows a where
    rRepr :: a -> [C8.ByteString]
    rRepr = (:[]) . C8.pack . show

printRows :: forall a . (Rows a) => Bool -> a -> IO ()
printRows b = mapM_ f . rRepr
    where f = if b then handleErrors . C8.putStrLn else C8.putStrLn

instance Rows Bool
instance Rows Double
instance Rows Float
instance Rows Int
instance Rows Integer
instance Rows ()

instance Rows Char where
    rRepr = (:[]) . C8.singleton

instance Rows ByteString where
    rRepr = (:[])

instance (Typeable a,Show a,Rows a) => Rows (Maybe a) where
    rRepr = maybe [] rRepr

instance (Row a, Row b) => Rows (a,b) where
    rRepr (x,y) = [rshow x,rshow y]

instance (Row a, Row b) => Rows (Map a b) where
    rRepr = listAsRows . M.toList

-- Lists

class (Row a) => ListAsRows a where
    listAsRows :: [a] -> [ByteString]
    listAsRows = L.map rshow

instance ListAsRows ByteString
instance ListAsRows Bool
instance ListAsRows Double
instance ListAsRows Float
instance ListAsRows Int
instance ListAsRows Integer
instance ListAsRows ()
instance (Row a,Row b) => ListAsRows (a,b)
instance (Row a,Row b,Row c) => ListAsRows (a,b,c)

instance ListAsRows Char where
    listAsRows = (:[]) . C8.pack

instance (ListAsRows a) => Rows [a] where
    rRepr = listAsRows

-- ---------------------------
-- Row class and instances

-- | Row is similar to Show, instances should be convertible
-- to string. The output of rshow should be formatted such that
-- it can be read and processed from the command line.
--
-- For example:
-- @
--    show [1,2,3,4] = "[1,2,3,4]"
--    rshow [1,2,3,4] = "1 2 3 4"
-- @
class (Typeable a, Show a) => Row a where
    rshow :: a -> ByteString
    rshow = C8.pack . show

instance Row Bool
instance Row Float
instance Row Double
instance Row Int
instance Row Integer
instance Row ()

instance Row Char where
    rshow = C8.singleton

printRow :: forall a . (Row a) => Bool -> a -> IO ()
printRow b = if b then handleErrors . f else f
                where f = C8.putStrLn . rshow

class (Show a) => ListRepresentation a where
    listRepr :: [a] -> ByteString
    listRepr = C8.intercalate " " . L.map (C8.pack . show)

instance ListRepresentation Bool
instance ListRepresentation Float
instance ListRepresentation Int
instance ListRepresentation Integer
instance ListRepresentation ()
instance (Show a) => ListRepresentation [a]

instance ListRepresentation Char where
    listRepr = C8.pack

instance ListRepresentation ByteString where
    listRepr = C8.intercalate " "

instance (Typeable a,ListRepresentation a) => Row [a] where
    rshow = listRepr

instance Row ByteString where
    rshow = id

instance (Row a,Row b) => Row (a,b) where
    rshow (a,b) = C8.intercalate "\t" [rshow a,rshow b]

instance (Row a,Row b,Row c) => Row (a,b,c) where
    rshow (a,b,c) = C8.intercalate "\t" [rshow a,rshow b,rshow c]
