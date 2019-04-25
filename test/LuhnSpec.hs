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
