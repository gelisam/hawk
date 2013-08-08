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

spec :: Spec
spec = describe "repr" $ do
    it "can convert boolean values" $ do
        repr True `shouldBe` ["True"]
        repr False `shouldBe` ["False"]

    it "can convert char values" $ example $
        repr 'c' `shouldBe` ["c"]

    it "can convert double values" $ example $
        repr (1.1::Double) `shouldBe` ["1.1"]

    it "can convert float values" $ example $
        repr (1.1::Float) `shouldBe` ["1.1"]

    it "can convert int values" $ example $
        repr (1::Int) `shouldBe` ["1"]

    it "can convert integer values" $ example $
        repr (1::Integer) `shouldBe` ["1"]

    it "can convert maybe values" $ do 
        example $ repr (Nothing::Maybe ()) `shouldBe` [""]
        example $ repr (Just 1::Maybe Int) `shouldBe` ["1"]
        example $ repr (Just (Just True)) `shouldBe` ["True"]

    it "can convert unit value" $
        repr () `shouldBe` [""]

    it "can convert string values" $ do
        example $ repr "" `shouldBe` [""]
        example $ repr "word" `shouldBe` ["word"]
        example $ repr "word word" `shouldBe` ["word word"]

    it "can convert tuple values" $ do
        example $ repr (1,True) `shouldBe` ["1","True"]
        example $ repr ((1,2),False) `shouldBe` ["1 2","False"]

    it "can convert list values" $ do
        repr ([]::[()]) `shouldBe` []
        example $ repr [True] `shouldBe` ["True"]
        example $ repr [True,False] `shouldBe` ["True","False"]
        example $ repr [Just 1,Nothing] `shouldBe` ["1",""]
        example $ repr [[1,2,3],[4,5,6]] `shouldBe` ["1 2 3","4 5 6"]
        example $ repr ["w w","w w"] `shouldBe` ["w w","w w"]
        example $ repr [["w w"],["w w"]] `shouldBe` ["w w","w w"]

    it "can convert map values" $ do
        repr (M.empty::Map Bool Bool) `shouldBe` []
        example $ repr (M.fromList [(1,2),(3,4)]) `shouldBe` ["1 2","3 4"]
        example $ repr ([M.fromList [(1,2),(3,4)]]) `shouldBe` ["1 2\t3 4"]

    it "can convert set values" $ do
        repr (S.empty::Set Bool) `shouldBe` []
        example $ repr (S.fromList [1,2,3,4]) `shouldBe` ["1","2","3","4"]
        example $ repr ([S.fromList [1,2]]) `shouldBe` ["1 2"]
