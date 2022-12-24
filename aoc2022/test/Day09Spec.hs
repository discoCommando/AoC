module Day09Spec where

import Common
import Day09 hiding (main)
import Test.Hspec
import Board (Position(..), divPosition)

-- import Text.RawString.QQ (r)

-- example1 =
--   unsafeParseExample
--     solution
--     [r| |]

spec =
  describe "all" $ do
    fit "all" $ do
      True `shouldBe` True
      False `shouldBe` False
      moveTail (Position 0 0) (Position 1 0) `shouldBe` Position 1 0
      moveTail (Position 0 0) (Position 1 1) `shouldBe` Position 1 1
      divPosition (Position (-2) (-1)) (Position 2 1) `shouldBe` Position (-1) (-1)
      divPosition (Position (-1) (-2)) (Position 2 1) `shouldBe` Position (-1) (-2)
      divPosition (Position (-2) (-2)) (Position 2 1) `shouldBe` Position (-1) (-2)
      moveTail (Position 0 0) (Position 2 1) `shouldBe` Position 1 0
