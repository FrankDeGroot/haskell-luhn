module LuhnSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Luhn
import Luhn.Internal

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "toDigits" $ do
    it "converts a number to a list of digits" $ do
      toDigits 1234567 `shouldBe` [1,2,3,4,5,6,7]
      toDigits 2468 `shouldBe` [2,4,6,8]

  describe "doubleEveryOther" $ do
    it "doubles every other number starting from the right" $ do
      doubleEveryOther [1,2,3,4,5] `shouldBe` [1,4,3,8,5]
      doubleEveryOther [1,2,3,4,5,6] `shouldBe` [2,2,6,4,10,6]

  describe "sumDigits" $ do
    context "when all numbers are less than 10" $ do
      it "sums the list of integers" $ do
        sumDigits [1,2,3,4,5,6] `shouldBe` sum [1,2,3,4,5,6]

    context "when some numbers are greater or equal to 10" $ do
      it "sums their digits first before summing the list" $ do
        -- 2+1+2+4+1+4+6+8 = 28
        sumDigits [2,12,4,14,6,8] `shouldBe` 28

  describe "toDigits" $ do
    it "holds on: x == fromDigits(toDigits x)" $ do
      property $ \x -> x >= 0 ==> x == (fromDigits . toDigits) x

  describe "fromDigits" $ do
    it "convert a list of digits to an integer" $ do
      fromDigits [] `shouldBe` 0
      fromDigits [1] `shouldBe` 1
      fromDigits [1,2,3] `shouldBe` 123

  describe "validate" $ do
    it "returns True if number is valid, False otherwise" $ do
      1234567889 `shouldSatisfy` validate
      1234567887 `shouldNotSatisfy` validate