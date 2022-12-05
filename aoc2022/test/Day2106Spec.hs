module Day2106Spec where

import Common
import Day2106 hiding (main)
import Test.Hspec

-- import Text.RawString.QQ (r)

example1 =
  unsafeParseExample
    solution
    "3,4,3,1,2"

spec =
  describe "all" $ do
    it "all" $ do
      True `shouldBe` True
      False `shouldBe` False
      length (grow 18 step example1) `shouldBe` 26
      length (grow 80 step example1) `shouldBe` 5934
      -- length (grow 256 step example1) `shouldBe` 26984457539
      count' (grow 0 stepBetter $ createMap example1) `shouldBe` 5
      count' (grow 1 stepBetter $ createMap example1) `shouldBe` 5
      count' (grow 2 stepBetter $ createMap example1) `shouldBe` 6
      count' (grow 18 stepBetter $ createMap example1) `shouldBe` 26
      count' (grow 80 stepBetter $ createMap example1) `shouldBe` 5934
      count' (grow 256 stepBetter $ createMap example1) `shouldBe` 26984457539
