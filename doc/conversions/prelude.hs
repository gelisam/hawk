import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List as L

to :: (Read a) => B.ByteString -> a
to = read . B.unpack

toBool :: B.ByteString -> Bool
toBool = to

toInt :: B.ByteString -> Int
toInt = to

toFloat :: B.ByteString -> Float
toFloat = to

toDouble :: B.ByteString -> Double
toDouble = to
