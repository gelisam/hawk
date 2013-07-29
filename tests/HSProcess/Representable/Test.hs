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
{-# LANGUAGE ExtendedDefaultRules,OverloadedStrings #-}
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

import Test.Hspec 

spec :: Spec
spec = describe "rRepr" $ do
    it "can convert boolean values" $ do
        rRepr True `shouldBe` ["True"]
        rRepr False `shouldBe` ["False"]

    it "can convert char values" $ example $
        rRepr 'c' `shouldBe` ["c"]

    it "can convert double values" $ example $
        rRepr (1.1::Double) `shouldBe` ["1.1"]

    it "can convert float values" $ example $
        rRepr (1.1::Float) `shouldBe` ["1.1"]

    it "can convert int values" $ example $
        rRepr (1::Int) `shouldBe` ["1"]

    it "can convert integer values" $ example $
        rRepr (1::Integer) `shouldBe` ["1"]

    it "can convert maybe values" $ do 
        example $ rRepr (Nothing::Maybe ()) `shouldBe` [""]
        example $ rRepr (Just 1::Maybe Int) `shouldBe` ["1"]
        example $ rRepr (Just (Just True)) `shouldBe` ["True"]

    it "can convert unit value" $
        rRepr () `shouldBe` [""]

    it "can convert string values" $ do
        example $ rRepr "" `shouldBe` [""]
        example $ rRepr "word" `shouldBe` ["word"]
        example $ rRepr "word word" `shouldBe` ["word word"]

    it "can convert tuple values" $ do
        example $ rRepr (1,True) `shouldBe` ["1","True"]
        example $ rRepr ((1,2),False) `shouldBe` ["1 2","False"]

    it "can convert list values" $ do
        rRepr ([]::[()]) `shouldBe` []
        example $ rRepr [True] `shouldBe` ["True"]
        example $ rRepr [True,False] `shouldBe` ["True","False"]
        example $ rRepr [Just 1,Nothing] `shouldBe` ["1",""]
        example $ rRepr [[1,2,3],[4,5,6]] `shouldBe` ["1 2 3","4 5 6"]
        example $ rRepr ["w w","w w"] `shouldBe` ["w w","w w"]
        example $ rRepr [["w w"],["w w"]] `shouldBe` ["w w","w w"]

    it "can convert map values" $ do
        rRepr (M.empty::Map Bool Bool) `shouldBe` []
        example $ rRepr (M.fromList [(1,2),(3,4)]) `shouldBe` ["1 2","3 4"]
        example $ rRepr ([M.fromList [(1,2),(3,4)]]) `shouldBe` ["1 2\t3 4"]

    it "can convert set values" $ do
        rRepr (S.empty::Set Bool) `shouldBe` []
        example $ rRepr (S.fromList [1,2,3,4]) `shouldBe` ["1","2","3","4"]
        example $ rRepr ([S.fromList [1,2]]) `shouldBe` ["1 2"]
