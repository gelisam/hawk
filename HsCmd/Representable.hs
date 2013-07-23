{-# LANGUAGE DeriveDataTypeable
           , ExistentialQuantification
           , OverloadedStrings
           , ScopedTypeVariables #-}
module HsCmd.Representable where

import Control.Exception (SomeException,handle)
import Data.ByteString.Lazy.Char8 as C8 hiding (hPutStrLn)
import Data.List as L
import qualified Data.Vector as V
import Data.Typeable (Typeable)
import System.IO (hPutStrLn,stderr)


handleErrors :: IO () -> IO ()
handleErrors = handle (\(e :: SomeException) -> hPutStrLn stderr (show e))

class (Typeable a, Show a) => RowShow a where
    rshow :: a -> ByteString
    rshow = C8.pack . show
    
    rprint :: Bool -> a -> IO ()
    rprint b x = if b then handle (\(e :: SomeException) -> return ()) f
                    else f
        where f = C8.putStr . C8.pack $ show x

instance RowShow Bool
instance RowShow Char
instance RowShow Float
instance RowShow Int
instance RowShow Integer
instance RowShow ()

instance RowShow ByteString where
    rshow = id
    rprint = const C8.putStr


class (Typeable a, Show a) => ColumnShow a where
    cshow :: a -> ByteString
    cshow = C8.pack . show

instance ColumnShow Bool
instance ColumnShow Char
instance ColumnShow Float
instance ColumnShow Int
instance ColumnShow Integer
instance ColumnShow ()


rowRepr :: forall a . (RowShow a) => Bool -> a -> IO ()
rowRepr = rprint

colRepr :: forall a . (ColumnShow a) => Bool -> a -> IO ()
colRepr b = if b then handleErrors . f
                 else f
                where f = C8.putStrLn . cshow

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

instance (Typeable a,ListRepresentation a) => ColumnShow [a] where
    cshow = listRepr

instance (ColumnShow a) => RowShow [a] where
    rshow = C8.intercalate "\n" . L.map cshow
    rprint b = if b
                  then mapM_ (handleErrors . f)
                  else mapM_ f
        where f = C8.putStrLn . cshow


instance ColumnShow ByteString where
    cshow = id

instance (Typeable a, Typeable b,Show a,Show b) => ColumnShow (a,b) where
    cshow (a,b) = C8.pack $ show a ++ ' ':show b

instance (Typeable a, Typeable b,Show a,Show b) => RowShow (a,b) where
    rshow (a,b) = C8.pack $ show a ++ '\n':show b
    rprint _ (a,b) = C8.putStr (C8.pack $ show a) >>
                     C8.putStr (C8.pack $ show b)

instance (Typeable a,ListRepresentation a) => ColumnShow (V.Vector a) where
    cshow = listRepr . V.toList

instance (ColumnShow a) => RowShow (V.Vector a) where
    rshow = C8.intercalate "\n" . V.toList . V.map cshow
