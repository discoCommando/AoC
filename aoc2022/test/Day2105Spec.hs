module Day2105Spec where

import Common
import Day2105 hiding (main)
import Test.Hspec
import qualified Text.Megaparsec as Mega
import Data.Proxy
import Parseable

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

      Mega.parse (parser (Proxy :: Proxy InputLine')) "i" "0,9 -> 5,9" `shouldBe` Right (((0,9),5),9)
      -- 10 `mod` 1 `shouldBe` 1
      -- -10 `mod` 1 `shouldBe` -1
      createLine InputLine {x1=0, y1=1, x2=0, y2=3} `shouldBe` [(0, 1), (0,2), (0,3)]
      createLine InputLine {x1=0, y1=1, x2=2, y2=3} `shouldBe` [(0, 1), (1,2), (2,3)]
      createLine InputLine {x1=2, y1=3, x2= -1, y2=0} `shouldBe` [(2, 3), (1,2), (0,1), (-1, 0)]
      createLine InputLine {x1=0, y1=1, x2=2, y2=1} `shouldBe` [(0, 1), (1,1), (2,1)]
