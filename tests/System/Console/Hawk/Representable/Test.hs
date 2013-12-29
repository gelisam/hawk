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

{-# LANGUAGE ExtendedDefaultRules,OverloadedStrings #-}
module System.Console.Hawk.Representable.Test where

import qualified Data.ByteString.Lazy.Char8 as C8

import qualified Test.Framework as TF
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import System.Console.Hawk.Representable

import Test.Hspec 


reprSpec' :: Spec
reprSpec' = describe "repr'" $ do
    it "can convert tuple values" $ do
      example $ repr' " " (1,True) `shouldBe` "1 True"
      example $ repr' " " (1,True,2) `shouldBe` "1 True 2"
      example $ repr' " " (1,2,3,4) `shouldBe` "1 2 3 4"
      example $ repr' " " (1,2,3,4,5) `shouldBe` "1 2 3 4 5"
      example $ repr' " " (1,2,3,4,5,6) `shouldBe` "1 2 3 4 5 6"
      example $ repr' " " (1,2,3,4,5,6,7) `shouldBe` "1 2 3 4 5 6 7"
      example $ repr' " " (1,2,3,4,5,6,7,8) `shouldBe` "1 2 3 4 5 6 7 8"
      example $ repr' " " (1,2,3,4,5,6,7,8,9) `shouldBe` "1 2 3 4 5 6 7 8 9"
      example $ repr' " " (1,2,3,4,5,6,7,8,9,10) `shouldBe` "1 2 3 4 5 6 7 8 9 10"

reprSpec :: Spec
reprSpec = describe "repr" $ do
    it "can convert boolean values" $ do
        repr "\t" True `shouldBe` ["True"]
        repr "\t" False `shouldBe` ["False"]

    it "can convert char values" $ example $
        repr "\t" 'c' `shouldBe` ["c"]

    it "can convert double values" $ example $
        repr "\t" (1.1::Double) `shouldBe` ["1.1"]

    it "can convert float values" $ example $
        repr "\t" (1.1::Float) `shouldBe` ["1.1"]

    it "can convert int values" $ example $
        repr "\t" (1::Int) `shouldBe` ["1"]

    it "can convert integer values" $ example $
        repr "\t" (1::Integer) `shouldBe` ["1"]

    it "can convert maybe values" $ do 
        example $ repr "\t" (Nothing::Maybe ()) `shouldBe` [""]
        example $ repr "\t" (Just 1::Maybe Int) `shouldBe` ["1"]
        example $ repr "\t" (Just (Just True)) `shouldBe` ["True"]

    it "can convert unit value" $
        repr "\t" () `shouldBe` [""]

    it "can convert string values" $ do
        example $ repr "\t" "" `shouldBe` [""]
        example $ repr "\t" "word" `shouldBe` ["word"]
        example $ repr "\t" "word word" `shouldBe` ["word word"]

    it "can convert tuple values" $ do
        example $ repr "\t" (1,True) `shouldBe` ["1","True"]
        example $ repr "\t" ((1,2),False) `shouldBe` ["1\t2","False"]
        example $ repr "\t" (1,2,3) `shouldBe` ["1","2","3"]
        example $ repr "\t" (1,2,3,4) `shouldBe` ["1","2","3","4"]
        example $ repr "\t" (1,2,3,4,5) `shouldBe` ["1","2","3","4","5"]
        example $ repr "\t" (1,2,3,4,5,6) `shouldBe` ["1","2","3","4","5","6"]
        example $ repr "\t" (1,2,3,4,5,6,7) `shouldBe` ["1","2","3","4","5","6","7"]
        example $ repr "\t" (1,2,3,4,5,6,7,8) `shouldBe` ["1","2","3","4","5","6","7","8"]
        example $ repr "\t" (1,2,3,4,5,6,7,8,9) `shouldBe` ["1","2","3","4","5","6","7","8","9"]
        example $ repr "\t" (1,2,3,4,5,6,7,8,9,10) `shouldBe` ["1","2","3","4","5","6","7","8","9","10"]

    it "can convert list values" $ do
        repr "\t" ([]::[()]) `shouldBe` []
        example $ repr "\t" [True] `shouldBe` ["True"]
        example $ repr "\t" [True,False] `shouldBe` ["True","False"]
        example $ repr "\t" [Just 1,Nothing] `shouldBe` ["1",""]
        example $ repr "\t" [[1,2,3],[4,5,6]] `shouldBe` ["1\t2\t3","4\t5\t6"]
        example $ repr "\t" ["w w","w w"] `shouldBe` ["w w","w w"]
        example $ repr "\t" [["w w"],["w w"]] `shouldBe` ["w w","w w"]

    it "can convert map values" $ do
        repr "\t" (M.empty::Map Bool Bool) `shouldBe` []
        example $ repr "\t" (M.fromList [(1,2),(3,4)]) `shouldBe` ["1\t2","3\t4"]
        example $ repr "\t" ([M.fromList [(1,2),(3,4)]]) `shouldBe` ["1 2\t3 4"]

    it "can convert set values" $ do
        repr "\t" (S.empty::Set Bool) `shouldBe` []
        example $ repr "\t" (S.fromList [1,2,3,4]) `shouldBe` ["1","2","3","4"]
        example $ repr "\t" ([S.fromList [1,2]]) `shouldBe` ["1\t2"]
