module Day15Spec where

import Common
import Day15 hiding (main)
import Test.Hspec
import Text.RawString.QQ (r)

example1 =
  unsafeParseExample
    solution
    [r|0,3,6|]

spec =
  describe "all" $ do
    it "all" $ do
      True `shouldBe` True
      False `shouldBe` False
      part1 solution example1 `shouldBe` 436
      part2 solution example1 `shouldBe` 175594
