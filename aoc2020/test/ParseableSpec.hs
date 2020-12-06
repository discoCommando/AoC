module ParseableSpec where

import Data.Data
import Parseable
import Test.Hspec
import qualified Text.Megaparsec as Mega

spec = do
  describe "all" $ do
    it "all" $ do
      True `shouldBe` True

      Mega.parse
        (parser (Proxy :: Proxy Digit))
        "a"
        "1"
        `shouldBe` Right '1'

      Mega.parse
        (parser (Proxy :: Proxy (Sized 3 Digit)))
        "a"
        "1234"
        `shouldBe` Right "123"

      Mega.parseMaybe
        (parser (Proxy :: Proxy (Sized 3 Digit)))
        "12"
        `shouldBe` Nothing

      Mega.parse
        (parser (Proxy :: Proxy (Const "abc" "xd")))
        "a"
        "abcd"
        `shouldBe` Right "xd"

      Mega.parse
        (parser (Proxy :: Proxy (Const "abc" "xd" :+: Const "xyz" "ab")))
        "a"
        "abcxyzg"
        `shouldBe` Right ("xd", "ab")

      Mega.parse
        (parser (Proxy :: Proxy (Const "abc" "xd" $> Const "xyz" "ab")))
        "a"
        "abcxyz"
        `shouldBe` Right "ab"

      Mega.parse
        (parser (Proxy :: Proxy (Const "abc" "xd" <$ Const "xyz" "ab")))
        "a"
        "abcxyz"
        `shouldBe` Right "xd"

      Mega.parse
        (parser (Proxy :: Proxy (Choice [Const "abc" "xd", Const "lfu" "db"])))
        "a"
        "abcxyz"
        `shouldBe` Right "xd"

      Mega.parse
        (parser (Proxy :: Proxy (Choice [Const "abc" "xd", Const "lfu" "db"])))
        "a"
        "lfuxx"
        `shouldBe` Right "db"

      Mega.parse
        (parser (Proxy :: Proxy (Const "abc" "xd" <||> Const "lfu" "db")))
        "a"
        "lfuxx"
        `shouldBe` Right (Right "db")

      Mega.parse
        (parser (Proxy :: Proxy (Const "abc" "xd" <||> Const "lfu" "db")))
        "a"
        "abcxyz"
        `shouldBe` Right (Left "xd")

      Mega.parse
        (parser (Proxy :: Proxy (Const "abc" "xd" <||> Const "lfu" "db")))
        "a"
        "abcxyz"
        `shouldBe` Right (Left "xd")

      Mega.parse
        (parser (Proxy :: Proxy (ToInt (Many Digit))))
        "a"
        "1"
        `shouldBe` Right 1

      Mega.parse
        (parser (Proxy :: Proxy (ToInt (Many Digit))))
        "a"
        "123"
        `shouldBe` Right 123

      Mega.parse
        (parser (Proxy :: Proxy (ToInt (Many Digit))))
        "a"
        "123a"
        `shouldBe` Right 123

      Mega.parse
        (parser (Proxy :: Proxy (ToInt (Many Digit))))
        "a"
        "0123a"
        `shouldBe` Right 123

      Mega.parse
        (parser (Proxy :: Proxy (SepEndBy (ToInt (Many Digit)) (Chunk ","))))
        "a"
        "0123,342,111"
        `shouldBe` Right [123, 342, 111]

      Mega.parse
        (parser (Proxy :: Proxy (SepEndBy (ToInt (Many Digit)) (Chunk ","))))
        "a"
        "0123"
        `shouldBe` Right [123]

      Mega.parse
        (parser (Proxy :: Proxy (LEQThan 100 (ToInt (Many Digit)))))
        "a"
        "40"
        `shouldBe` Right 40

      Mega.parse
        (parser (Proxy :: Proxy (LEQThan 100 (ToInt (Many Digit)))))
        "a"
        "100"
        `shouldBe` Right 100

      Mega.parseMaybe
        (parser (Proxy :: Proxy (LEQThan 100 (ToInt (Many Digit)))))
        "101"
        `shouldBe` Nothing

      Mega.parse
        (parser (Proxy :: Proxy (BEQThan 100 (ToInt (Many Digit)))))
        "a"
        "120"
        `shouldBe` Right 120

      Mega.parse
        (parser (Proxy :: Proxy (BEQThan 100 (ToInt (Many Digit)))))
        "a"
        "100"
        `shouldBe` Right 100

      Mega.parseMaybe
        (parser (Proxy :: Proxy (BEQThan 100 (ToInt (Many Digit)))))
        "99"
        `shouldBe` Nothing

      Mega.parse
        (parser (Proxy :: Proxy Space))
        "a"
        " x"
        `shouldBe` Right ' '

      Mega.parse
        (parser (Proxy :: Proxy Newline))
        "a"
        "\n"
        `shouldBe` Right '\n'

      Mega.parse
        (parser (Proxy :: Proxy (Join (Sized 1 (Many Space)))))
        "a"
        "          "
        `shouldBe` Right "          "

      Mega.parse
        (parser (Proxy :: Proxy (NotRequired Digit)))
        "a"
        "9"
        `shouldBe` Right (Just '9')

      Mega.parse
        (parser (Proxy :: Proxy (NotRequired Digit)))
        "a"
        "a"
        `shouldBe` Right Nothing

      Mega.parse
        (parser (Proxy :: Proxy (Many NotWhitespace)))
        "a"
        "aarstarstarst "
        `shouldBe` Right "aarstarstarst"
