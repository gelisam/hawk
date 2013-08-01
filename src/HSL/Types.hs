{-# LANGUAGE OverloadedStrings,FlexibleInstances,UndecidableInstances,OverlappingInstances #-}

module HSL.Types where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (intersperse)


-- Type hints

tS :: B.ByteString
tS = undefined

tI :: Int
tI = undefined


class (Show a) => Scalar a where
    tb :: a => B.ByteString
    tb = B.pack . show

class Datum a where
    ser :: a -> B.ByteString

class Renderable a where
    render :: a -> [B.ByteString]


instance Scalar Int
instance Scalar Integer
instance Scalar Float
instance Scalar Char
instance Scalar String where tb = B.pack
instance Scalar B.ByteString where tb = id

instance (Scalar a) => Datum a where
    ser = tb
instance (Scalar a, Scalar b) => Datum (a, b) where
    ser (a, b) = B.concat $ intersperse "\t" $ [tb a, tb b]
instance (Scalar a, Scalar b, Scalar c) => Datum (a, b, c) where
    ser (a, b, c) = B.concat $ intersperse "\t" $ map tb [tb a, tb b, tb c]
instance (Scalar a, Scalar b, Scalar c, Scalar d) => Datum (a, b, c, d) where
    ser (a, b, c, d) = B.concat $ intersperse "\t" $ map tb [tb a, tb b, tb c, tb d]
instance (Scalar a, Scalar b, Scalar c, Scalar d, Scalar e) => Datum (a, b, c, d, e) where
    ser (a, b, c, d, e) = B.concat $ intersperse "\t" $ map tb [tb a, tb b, tb c, tb d, tb e]

instance (Datum a) => Renderable [a] where
    render = map ser
instance (Datum a) => Renderable [[a]] where
    render = render . concat
instance (Datum a) => Renderable a where
    render = (:[]) . ser
