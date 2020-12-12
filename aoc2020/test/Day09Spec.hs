module Day09Spec where

import Common
import Day09 hiding (main)
import Test.Hspec
import Text.RawString.QQ (r)

example1 =
  unsafeParseExample
    solution
    [r|35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576|]

spec =
  describe "all" $ do
    it "all" $ do
      True `shouldBe` True
      False `shouldBe` False
      part1' 5 example1 `shouldBe` 127
      part2' 5 example1 `shouldBe` 62
