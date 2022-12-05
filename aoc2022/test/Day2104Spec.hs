module Day2104Spec where

import Common
import Data.Data (Proxy (..))
import Day2104 hiding (main)
import Parseable
import Test.Hspec
import qualified Text.Megaparsec as Mega

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
      Mega.parse (parser (Proxy :: Proxy Guesses')) "i" "1,2\n" `shouldBe` Right [1, 2]
      Mega.parse (parser (Proxy :: Proxy InputLine')) "i" "1,2\n" `shouldBe` Right ([1, 2], [])
      Mega.parse (parser (Proxy :: Proxy (Newline $> Digit))) "i" "\n1,2\n" `shouldBe` Right '1'

-- Mega.parse (parser (Proxy :: Proxy Board' )) "i" "\n22 13 17 11  0\n " `shouldBe` Right []
-- Mega.parse
--   ( parser
--       ( Proxy ::
--           Proxy
--             (Many Space $> SepEndBy ( ToInt (Some Digit) ) (Many Space)
--             <$ Newline
--             )
--       )
--   )
--   "i"
--   "22 23   24\n"
--   `shouldBe` Right [22, 23]
