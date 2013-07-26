{- 
  Copyright 2013 Mario Pastorelli (pastorelli.mario@gmail.com)
 
    This file is part of HSProcess.
 
  HSProcess is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.
 
  HSProcess is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with HSProcess.  If not, see <http://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ExtendedDefaultRules #-}
module HSProcess.Representable.Test where

import qualified Data.ByteString.Lazy.Char8 as C8

import qualified Test.Framework as TF
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import HSProcess.Representable

tests :: [TF.Test]
tests = concatMap hUnitTestToTests
        [test_rReprs
        ]

test_rReprs :: Test
test_rReprs =
   "rRepr" `should_convert` [
          True          `into`      "True"
        , False         `into`      "False"
        , 'a'           `into`      "a"
        , (1.1::Double) `into`      "1.1"
        , (1.1::Float)  `into`      "1.1"
        , (1::Int)      `into`      "1"
        , (1::Integer)  `into`      "1"
        , typedNothing  `into`      ""
        , Just True     `into`      "True"
        , Just (Just 1) `into`      "1"
        , ""            `into`      ""
        , "word"        `into`      "word"
        , "word word"   `into`      "word word"
        , ()            `into`      ""
        , (1,2)         `into_list` ["1","2"]
        , ((1,2),(2,3)) `into_list` ["1 2","2 3"] 

        -- List
        , ([]::[Bool])          `into_list` []
        , [True]                `into`      "True"
        , [True,False]          `into_list` ["True","False"]
        , [Just 1,typedNothing] `into_list` ["1",""]
        , [[1,2,3],[4,5,6]]     `into_list` ["1 2 3","4 5 6"]
        , ["w w","w w"]         `into_list` ["w w","w w"]
        , [["w w"],["w w"]]     `into_list` ["w w","w w"]

    
        -- Map
        , (M.empty::Map Bool Bool)   `into_list` []
        , M.fromList [(1,2),(3,4)]   `into_list` ["1 2", "3 4"]
        , [M.fromList [(1,2),(3,4)]] `into`      "1 2\t3 4"

        -- Set
        , (S.empty::Set Bool)    `into_list` []
        , S.fromList [1,1,2,3,4] `into_list` ["1","2","3","4"]
        , [S.fromList [1,2]]     `into`      "1 2"
    ]
    where typedNothing = Nothing::Maybe Int

          should_convert :: String -> [Test] -> Test
          should_convert s = TestLabel s . TestList
          into :: (Rows t) => t -> String -> Test
          into t s = TestLabel (t `to` s) $ mk_rRepr_test''' t s
          infix 9 `into`

          into_list :: (Rows t) => t -> [String] -> Test
          into_list t s = TestLabel (t `to` s) $ mk_rRepr_test'' t s
          infix 9 `into_list`

          to t s = show t ++ " -> " ++ show s

testList :: String -> [(String,Test)] -> Test
testList label = TestLabel label . TestList . map (uncurry TestLabel)

should :: String -> [(String,Test)] -> Test
should = testList

mk_rRepr_test :: (Rows t) => String -> t -> [String] -> Test
mk_rRepr_test d l r = TestCase (assertEqual d (rRepr l) (map C8.pack r))

mk_rRepr_test' :: (Rows t) => String -> t -> String -> Test
mk_rRepr_test' d l r = mk_rRepr_test d l [r]

mk_rRepr_test'' :: (Rows t) => t -> [String] -> Test
mk_rRepr_test'' l r = mk_rRepr_test d l r
    where d = "rRepr " ++ show l ++ " is " ++ show r

mk_rRepr_test''' :: (Rows t) => t -> String -> Test
mk_rRepr_test''' l r = mk_rRepr_test'' l [r]
