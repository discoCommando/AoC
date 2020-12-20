module Day14Spec where

import Common
import Day14 hiding (main)
import Test.Hspec
import Text.RawString.QQ (r)

example1 =
  unsafeParseExample
    solution
    [r|mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0|]

example2 =
  unsafeParseExample
    solution
    [r|mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1|]

spec =
  describe "all" $ do
    it "all" $ do
      True `shouldBe` True
      False `shouldBe` False
      part1 solution example1 `shouldBe` 165
      part2 solution example2 `shouldBe` 208
