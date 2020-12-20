module Day13Spec where

import Common
import Day13 hiding (main)
import Test.Hspec
import Text.RawString.QQ (r)

example1 =
  unsafeParseExample
    solution
    [r|939
7,13,x,x,59,x,31,19|]

example2 =
  unsafeParseExample
    solution
    [r|939
1789,37,47,1889|]

spec =
  describe "all" $ do
    it "all" $ do
      True `shouldBe` True
      False `shouldBe` False
      part1 solution example1 `shouldBe` 295
      part2 solution example1 `shouldBe` 1068781
      part2 solution example2 `shouldBe` 1202161486

{-
x mod 7 = 0
x mod 13 = 1

7 * 13 = 91
-}
