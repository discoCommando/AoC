module Day01Spec where

import Common
import Day01
import Test.Hspec
import Text.RawString.QQ

example1 =
  unsafeParseExample
    solution
    [r|1721
979
366
299
675
1456|]

spec = do
  describe "all" $ do
    it "all" $ do
      solution1 example1 `shouldBe` 514579
      solution2 example1 `shouldBe` 241861950
