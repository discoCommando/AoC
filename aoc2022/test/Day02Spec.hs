module Day02Spec where

import Common
import Day00 hiding (main)
import Test.Hspec

-- import NeatInterpolation
-- import Text.RawString.QQ (r)

-- example1 =
--   unsafeParseExample
--     solution
--     [text| |]

spec =
  describe "all" $ do
    it "all" $ do
      True `shouldBe` True
      False `shouldBe` False
