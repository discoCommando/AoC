module Day05Spec where

import Common
import Day05 hiding (main)
import Test.Hspec
import qualified Text.Megaparsec as Mega
import Text.RawString.QQ (r)

example1 =
  unsafeParseExample
    solution
    [r|BFFFBBFRRR
FFFBBBFRRR
BBFFBBFRLL|]

spec =
  describe "all" $ do
    it "all" $ do
      True `shouldBe` True
      False `shouldBe` False
      Mega.parse (parse solution) "a" "BFFFBBFRRR" `shouldBe` Right [([B, F, F, F, B, B, F], [R, R, R])]
      toNumber [F, B, F, B, B, F, F] `shouldBe` 44
      toNumber [R, L, R]
        `shouldBe` 5
      part1
        solution
        example1
        `shouldBe` 820
