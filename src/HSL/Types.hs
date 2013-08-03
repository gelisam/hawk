{-# LANGUAGE OverloadedStrings,FlexibleInstances,UndecidableInstances,OverlappingInstances #-}

module HSL.Types where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (intersperse)
import Data.Int (Int64)


-- Type hints

tS :: B.ByteString
tS = undefined

tI :: Int
tI = undefined

tF :: Float
tF = undefined


class Datum a where
    bs :: a -> B.ByteString

class Renderable a where
    render :: a -> [B.ByteString]


instance Datum a => Datum (Maybe a) where
    bs (Just a) = B.concat ["Just ", bs a]
    bs Nothing = "Nothing"

instance Datum Int where bs = B.pack . show
instance Datum Int64 where bs = B.pack . show
instance Datum Char where bs = B.pack . show
instance Datum String where bs = B.pack
instance Datum B.ByteString where bs = id

instance (Datum a, Datum b) => Datum (a, b) where
    bs (a, b) = B.concat $ intersperse "\t" [bs a, bs b]
instance (Datum a, Datum b, Datum c) => Datum (a, b, c) where
    bs (a, b, c) = B.concat $ intersperse "\t" [bs a, bs b, bs c]
instance (Datum a, Datum b, Datum c, Datum d) => Datum (a, b, c, d) where
    bs (a, b, c, d) = B.concat $ intersperse "\t" [bs a, bs b, bs c, bs d]
instance (Datum a, Datum b, Datum c, Datum d, Datum e) => Datum (a, b, c, d, e) where
    bs (a, b, c, d, e) = B.concat $ intersperse "\t" [bs a, bs b, bs c, bs d, bs e]

instance (Datum a) => Renderable a where
    render = (:[]) . bs
instance (Datum a) => Renderable [a] where
    render = map bs
instance (Datum a) => Renderable [[a]] where
    render = map bs . concat
