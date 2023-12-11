module Day05Spec where

import Common
import Day05 hiding (main)
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
