module Day06Spec where

import Common
import Day06 hiding (main)
import Test.Hspec
import Text.RawString.QQ (r)

example1 =
  unsafeParseExample
    solution
    [r|abc

a
b
c

ab
ac

a
a
a
a

b|]

spec =
  describe "all" $ do
    it "all" $ do
      True `shouldBe` True
      False `shouldBe` False
      example1 `shouldBe` [["abc"], ["a", "b", "c"], ["ab", "ac"], ["a", "a", "a", "a"], ["b"]]
      part1 solution example1 `shouldBe` 11
      part2 solution example1 `shouldBe` 6
