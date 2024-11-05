module Day11Spec where

import Common
import Day11 hiding (main)
import Test.Hspec

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
      let monke0 = Monkey {id = 0, items = [79, 98], operation = (Mult, [Old, Const 19]), test = DivisibleBy 23, throw = Throw {ifTrue = 2, ifFalse = 3}}

      operation' monke0.operation 79 `shouldBe` 500
      operation' monke0.operation 98 `shouldBe` 620
      test' monke0.test monke0.throw 500 `shouldBe` 3
