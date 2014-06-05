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

-- | Used by Hawk's runtime to format the output of a Hawk expression.
--    You can use this from your user prelude if you want Hawk to print
--    your custom datatypes in a console-friendly format.
module System.Console.Hawk.Representable (

    ListAsRow (listRepr')
  , ListAsRows (listRepr)
  , Row  (repr')
  , Rows (repr)

) where

import Prelude
import qualified Data.List as L
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text.Lazy as T


-- $setup
-- >>> import qualified Data.Text.Lazy.IO as TextIO

-- | A type that instantiate ListAsRow is a type that has a representation
-- when is embedded inside a list
--
-- For example:
--
-- >>> mapM_ TextIO.putStrLn $ repr T.empty "test"
-- test
class (Show a) => ListAsRow a where
    listRepr' :: T.Text -> [a] -> T.Text
    listRepr' d = T.intercalate d . L.map (T.pack . show)

instance ListAsRow Bool
instance ListAsRow Float
instance ListAsRow Double
instance ListAsRow Int
instance ListAsRow Integer
instance ListAsRow ()

instance (ListAsRow a) => ListAsRow [a] where
    -- todo check the first delimiter if it should be d
    listRepr' d = T.intercalate d . L.map (listRepr' d)

instance (Row a) => ListAsRow (Maybe a) where
    listRepr' d = T.intercalate d . L.map (repr' d)

instance (ListAsRow a) => ListAsRow (Set a) where
    listRepr' d = listRepr' d . L.map (listRepr' d . S.toList)

instance ListAsRow Char where
    listRepr' _ = T.pack

instance ListAsRow T.Text where
    listRepr' d = T.intercalate d

instance (Row a, Row b) => ListAsRow (Map a b) where
    listRepr' d = listRepr' d . L.map (listRepr' d . M.toList)

instance (Row a,Row b) => ListAsRow (a,b) where
    listRepr' d = T.intercalate d . L.map (\(x,y) -> T.unwords
                  [repr' d x,repr' d y])

instance (Row a,Row b,Row c) => ListAsRow (a,b,c) where
    listRepr' d = T.intercalate d . L.map (\(x,y,z) -> T.unwords
                  [repr' d x,repr' d y,repr' d z])

instance (Row a,Row b,Row c,Row d) => ListAsRow (a,b,c,d) where
    listRepr' d = T.intercalate d . L.map (\(a,b,c,e) -> T.unwords
                  [repr' d a,repr' d b,repr' d c,repr' d e])

instance (Row a,Row b,Row c,Row d,Row e) => ListAsRow (a,b,c,d,e) where
    listRepr' d = T.intercalate d . L.map (\(a,b,c,e,f) -> T.unwords
                  [repr' d a,repr' d b,repr' d c,repr' d e,repr' d f])

instance (Row a,Row b,Row c,Row d,Row e,Row f) => ListAsRow (a,b,c,d,e,f) where
    listRepr' d = T.intercalate d . L.map (\(a,b,c,e,f,g) -> T.unwords
                  [repr' d a,repr' d b,repr' d c,repr' d e,repr' d f
                  ,repr' d g])

instance (Row a,Row b,Row c,Row d,Row e,Row f,Row g)
  => ListAsRow (a,b,c,d,e,f,g) where
    listRepr' d = T.intercalate d . L.map (\(a,b,c,e,f,g,h) -> T.unwords
                  [repr' d a,repr' d b,repr' d c,repr' d e,repr' d f
                  ,repr' d g,repr' d h])

instance (Row a,Row b,Row c,Row d,Row e,Row f,Row g,Row h)
  => ListAsRow (a,b,c,d,e,f,g,h) where
    listRepr' d = T.intercalate d . L.map (\(a,b,c,e,f,g,h,i) -> T.unwords
                  [repr' d a,repr' d b,repr' d c,repr' d e,repr' d f
                  ,repr' d g,repr' d h,repr' d i])

instance (Row a,Row b,Row c,Row d,Row e,Row f,Row g,Row h,Row i)
  => ListAsRow (a,b,c,d,e,f,g,h,i) where
    listRepr' d = T.intercalate d . L.map (\(a,b,c,e,f,g,h,i,l) -> T.unwords
                  [repr' d a,repr' d b,repr' d c,repr' d e,repr' d f
                  ,repr' d g,repr' d h,repr' d i,repr' d l])

instance (Row a,Row b,Row c,Row d,Row e,Row f,Row g,Row h,Row i,Row l)
  => ListAsRow (a,b,c,d,e,f,g,h,i,l) where
    listRepr' d = T.intercalate d . L.map (\(a,b,c,e,f,g,h,i,l,m) -> T.unwords
                  [repr' d a,repr' d b,repr' d c,repr' d e,repr' d f
                  ,repr' d g,repr' d h,repr' d i,repr' d l,repr' d m])



-- | A Row is something that can be expressed as a record.
-- The output of repr' should be formatted such that
-- it can be read and processed from the command line.
--
-- For example:
--
-- >>> putStrLn $ show [1,2,3,4]
-- [1,2,3,4]
--
-- >>> TextIO.putStrLn $ repr' (T.pack " ") [1,2,3,4]
-- 1 2 3 4
class (Show a) => Row a where
    repr' :: T.Text -- ^ columns delimiter
          -> a           -- ^ value to represent
          -> T.Text
    repr' _ = T.pack . show

instance Row Bool
instance Row Float
instance Row Double
instance Row Int
instance Row Integer
instance Row ()

instance Row Char where
    repr' _ = T.singleton

instance (ListAsRow a) => Row [a] where
    repr' = listRepr'

instance (ListAsRow a) => Row (Set a) where
    repr' d = listRepr' d . S.toList

instance (Row a,Row b) => Row (Map a b) where
    repr' d = listRepr' d . M.toList

instance Row T.Text where
    repr' _ = id

instance (Row a) => Row (Maybe a) where
    repr' _ Nothing = T.empty
    repr' d (Just x) = repr' d x -- check if d is correct here

instance (Row a,Row b) => Row (a,b) where
    repr' d (a,b) = repr' d a `T.append` (d `T.append` repr' d b)
    --repr' d (a,b) = repr' d [repr' d a,repr' d b]

instance (Row a,Row b,Row c) => Row (a,b,c) where
    repr' d (a,b,c) =  repr' d a `T.append` (d `T.append`
                      (repr' d b `T.append` (d `T.append` repr' d c)))

instance (Row a,Row b,Row c,Row d) => Row (a,b,c,d) where
    repr' d (a,b,c,e) = repr' d a `T.append` (d `T.append`
                        (repr' d b `T.append` (d `T.append`
                        (repr' d c `T.append` (d `T.append` repr' d e)))))

instance (Row a,Row b,Row c,Row d,Row e) => Row (a,b,c,d,e) where
    repr' d (a,b,c,e,f) = repr' d a `T.append` (d `T.append`
                        (repr' d b `T.append` (d `T.append`
                        (repr' d c `T.append` (d `T.append`
                        (repr' d e `T.append` (d `T.append` repr' d f)))))))

instance (Row a,Row b,Row c,Row d,Row e,Row f) => Row (a,b,c,d,e,f) where
    repr' d (a,b,c,e,f,g) = repr' d a `T.append` (d `T.append`
                            (repr' d b `T.append` (d `T.append`
                            (repr' d c `T.append` (d `T.append`
                            (repr' d e `T.append` (d `T.append`
                            (repr' d f `T.append` (d `T.append` repr' d g)))))))))

instance (Row a,Row b,Row c,Row d,Row e,Row f,Row g) => Row (a,b,c,d,e,f,g) where
    repr' d (a,b,c,e,f,g,h) = repr' d a `T.append` (d `T.append`
                              (repr' d b `T.append` (d `T.append`
                              (repr' d c `T.append` (d `T.append`
                              (repr' d e `T.append` (d `T.append`
                              (repr' d f `T.append` (d `T.append`
                              (repr' d g `T.append` (d `T.append` repr' d h)))))))))))

instance (Row a,Row b,Row c,Row d,Row e,Row f,Row g,Row h)
        => Row (a,b,c,d,e,f,g,h) where
    repr' d (a,b,c,e,f,g,h,i) =
        repr' d a `T.append` (d `T.append`
       (repr' d b `T.append` (d `T.append`
       (repr' d c `T.append` (d `T.append`
       (repr' d e `T.append` (d `T.append`
       (repr' d f `T.append` (d `T.append`
       (repr' d g `T.append` (d `T.append`
       (repr' d h `T.append` (d `T.append` repr' d i)))))))))))))

instance (Row a,Row b,Row c,Row d,Row e,Row f,Row g,Row h,Row i)
        => Row (a,b,c,d,e,f,g,h,i) where
    repr' d (a,b,c,e,f,g,h,i,l) =
        repr' d a `T.append` (d `T.append`
       (repr' d b `T.append` (d `T.append`
       (repr' d c `T.append` (d `T.append`
       (repr' d e `T.append` (d `T.append`
       (repr' d f `T.append` (d `T.append`
       (repr' d g `T.append` (d `T.append`
       (repr' d h `T.append` (d `T.append`
       (repr' d i `T.append` (d `T.append` repr' d l)))))))))))))))

instance (Row a,Row b,Row c,Row d,Row e,Row f,Row g,Row h,Row i,Row l)
        => Row (a,b,c,d,e,f,g,h,i,l) where
    repr' d (a,b,c,e,f,g,h,i,l,m) =
        repr' d a `T.append` (d `T.append`
       (repr' d b `T.append` (d `T.append`
       (repr' d c `T.append` (d `T.append`
       (repr' d e `T.append` (d `T.append`
       (repr' d f `T.append` (d `T.append`
       (repr' d g `T.append` (d `T.append`
       (repr' d h `T.append` (d `T.append`
       (repr' d i `T.append` (d `T.append`
       (repr' d l `T.append` (d `T.append` repr' d m)))))))))))))))))


-- | A type that instantiate ListAsRows is a type that has a representation
-- when is embedded inside a list
--
-- Note: we use this class for representing a list of chars as String
-- instead of the standard list representation. Without this repr "test" would
-- yield ['t','e','s','r'] instead of "test".
--
-- For example:
--
-- >>> mapM_ TextIO.putStrLn $ repr T.empty "test"
-- test
class (Row a) => ListAsRows a where
    listRepr :: T.Text          -- ^ column delimiter
               -> [a]           -- ^ list of values to represent
               -> [T.Text]
    listRepr d = L.map (repr' d)

instance ListAsRows T.Text
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
instance (Row a,Row b,Row c,Row d) => ListAsRows (a,b,c,d)
instance (Row a,Row b,Row c,Row d,Row e) => ListAsRows (a,b,c,d,e)
instance (Row a,Row b,Row c,Row d,Row e,Row f) => ListAsRows (a,b,c,d,e,f)
instance (Row a,Row b,Row c,Row d,Row e,Row f,Row g) => ListAsRows (a,b,c,d,e,f,g)
instance (Row a,Row b,Row c,Row d,Row e,Row f,Row g,Row h)
  => ListAsRows (a,b,c,d,e,f,g,h)
instance (Row a,Row b,Row c,Row d,Row e,Row f,Row g,Row h,Row i)
  => ListAsRows (a,b,c,d,e,f,g,h,i)
instance (Row a,Row b,Row c,Row d,Row e,Row f,Row g,Row h,Row i,Row l)
  => ListAsRows (a,b,c,d,e,f,g,h,i,l)

instance ListAsRows Char where
    listRepr _ = (:[]) . T.pack

instance (ListAsRow a,ListAsRows a) => ListAsRows (Set a) where
    listRepr d = listRepr d . L.map S.toList

instance (Row a,Row b) => ListAsRows (Map a b) where
    listRepr d = listRepr d . L.map M.toList

instance (ListAsRows a) => Rows [a] where
    repr = listRepr


-- | A type that instantiate Rows is a type that can be represented as
-- a list of rows, where typically a row is a line.
--
-- For example:
--
-- >>> mapM_ TextIO.putStrLn $ repr (T.singleton '\n') [1,2,3,4]
-- 1
-- 2
-- 3
-- 4
class (Show a) => Rows a where
    -- | Return a representation of the given value as list of strings.
    repr :: T.Text    -- ^ rows delimiter
         -> a         -- ^ value to represent
         -> [T.Text]
    repr _ = (:[]) . T.pack . show


instance Rows Bool
instance Rows Double
instance Rows Float
instance Rows Int
instance Rows Integer

instance Rows () where
    repr _ = const [T.empty]

instance Rows Char where
    repr _ = (:[]) . T.singleton

instance Rows T.Text where
    repr _ = (:[])

instance (Rows a) => Rows (Maybe a) where
    repr d = maybe [T.empty] (repr d)

instance (Row a, Row b) => Rows (Map a b) where
    repr d = listRepr d . M.toList

instance (ListAsRows a) => Rows (Set a) where
    repr d = listRepr d . S.toList

instance (Row a, Row b) => Rows (a,b) where
    repr d (x,y) = [repr' d x,repr' d y]

instance (Row a, Row b, Row c) => Rows (a,b,c) where
    repr d (a,b,c) = [repr' d a, repr' d b, repr' d c]

instance (Row a, Row b, Row c, Row d) => Rows (a,b,c,d) where
    repr d (a,b,c,e) = [repr' d a, repr' d b, repr' d c, repr' d e]

instance (Row a, Row b, Row c, Row d, Row e) => Rows (a,b,c,d,e) where
    repr d (a,b,c,e,f) = [repr' d a, repr' d b, repr' d c, repr' d e, repr' d f]

instance (Row a, Row b, Row c, Row d, Row e, Row f) => Rows (a,b,c,d,e,f) where
    repr d (a,b,c,e,f,g) = [repr' d a, repr' d b, repr' d c,repr' d e
                           ,repr' d f, repr' d g]

instance (Row a, Row b, Row c, Row d, Row e, Row f, Row g)
       => Rows (a,b,c,d,e,f,g) where
    repr d (a,b,c,e,f,g,h) = [repr' d a, repr' d b, repr' d c,repr' d e
                             ,repr' d f, repr' d g, repr' d h]

instance (Row a, Row b, Row c, Row d, Row e, Row f, Row g, Row h)
       => Rows (a,b,c,d,e,f,g,h) where
    repr d (a,b,c,e,f,g,h,i) = [repr' d a, repr' d b, repr' d c, repr' d e
                               ,repr' d f, repr' d g, repr' d h, repr' d i]

instance (Row a, Row b, Row c, Row d, Row e, Row f, Row g, Row h, Row i)
       => Rows (a,b,c,d,e,f,g,h,i) where
    repr d (a,b,c,e,f,g,h,i,l) = [repr' d a, repr' d b, repr' d c, repr' d e
                                 ,repr' d f, repr' d g, repr' d h, repr' d i
                                 , repr' d l]

instance (Row a, Row b, Row c, Row d, Row e, Row f, Row g, Row h, Row i, Row l)
       => Rows (a,b,c,d,e,f,g,h,i,l) where
    repr d (a,b,c,e,f,g,h,i,l,m) = [repr' d a, repr' d b, repr' d c, repr' d e
                                   ,repr' d f, repr' d g, repr' d h, repr' d i
                                   ,repr' d l, repr' d m]
