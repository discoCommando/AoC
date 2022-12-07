module Day06Spec where

import Common
import Day06 hiding (main)
import Test.Hspec

-- import Text.RawString.QQ (r)

example1 =
  unsafeParseExample
    solution
    "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

spec =
  describe "all" $ do
    it "all" $ do
      True `shouldBe` True
      False `shouldBe` False
      part1' example1 `shouldBe` 7
