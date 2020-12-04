module Day02Spec where

import Common
import Day02
import Test.Hspec
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega
import Text.RawString.QQ (r)

example1 =
  [r|1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc|]

example1Parsed = unsafeParseExample solution example1

aa = unsafeMaybe $ Mega.parseMaybe parser "1-3 a: abcde"

spec =
  describe "all" $ do
    it "all" $ do
      aa `shouldBe` InputLine {password = "abcde", policy = Policy {min = 1, max = 3, letter = 'a'}}
      solution1 example1Parsed `shouldBe` 2

      validInput2 aa `shouldBe` True
      solution2 example1Parsed `shouldBe` 1
      fmap validInput2 example1Parsed `shouldBe` [True, False, False]
