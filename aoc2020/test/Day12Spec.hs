module Day12Spec where

import Common
import Day12 hiding (main)
import Test.Hspec
import Text.RawString.QQ (r)

example1 =
  unsafeParseExample
    solution
    [r|F10
N3
F7
R90
F11|]

spec =
  describe "all" $ do
    it "all" $ do
      True `shouldBe` True
      False `shouldBe` False
      part1 solution example1 `shouldBe` 25
      part2 solution example1 `shouldBe` 286
