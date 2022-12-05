module Day03Spec where

import Common
import Day03 hiding (main)
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
      priority 'a' `shouldBe` 1
      priority 'b' `shouldBe` 2
      priority 'z' `shouldBe` 26
      priority 'A' `shouldBe` 27
      priority 'Z' `shouldBe` 52
