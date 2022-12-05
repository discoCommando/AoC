module Day2103Spec where

import Common
import Day2103 hiding (main)
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
      listOfBoolsToInt [False, True, True] `shouldBe` 6
      listOfBoolsToInt [True, False, False, True] `shouldBe` 9
      listOfBoolsToInt [False] `shouldBe` 0
      listOfBoolsToInt [True] `shouldBe` 1
      listOfBoolsToInt [False, True] `shouldBe` 2
      listOfBoolsToInt [True, True] `shouldBe` 3
