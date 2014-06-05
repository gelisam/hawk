import qualified Data.Text as T
import qualified Data.List as L

to :: (Read a) => T.Text -> a
to = read . T.unpack

toBool :: T.Text -> Bool
toBool = to

toInt :: T.Text -> Int
toInt = to

toFloat :: T.Text -> Float
toFloat = to

toDouble :: T.Text -> Double
toDouble = to
