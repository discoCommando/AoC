module Day10Spec where

import Common
import Day10 hiding (main)
import Test.Hspec

-- import Text.RawString.QQ (r)

-- example1 =
--   unsafeParseExample
--     solution
--     [r| |]

spec =
  describe "all" $ do
    it "all" $ do
      True `shouldBe` True
      False `shouldBe` False
      [20, 60 .. 100] `shouldBe` [20, 60, 100]
