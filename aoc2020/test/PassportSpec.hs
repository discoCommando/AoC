module PassportSpec where

import Data.Data
import Parseable
import Passport
import Test.Hspec
import qualified Text.Megaparsec as Mega

spec = do
  describe "all" $ do
    it "all" $ do
      True `shouldBe` True

      Mega.parse
        (parser (Proxy :: Proxy (FieldValue BirthYear)))
        "a"
        "1920"
        `shouldBe` Right 1920
      Mega.parse
        (parser (Proxy :: Proxy (FieldValue BirthYear)))
        "a"
        "2002"
        `shouldBe` Right 2002
      Mega.parseMaybe
        (parser (Proxy :: Proxy (FieldValue BirthYear)))
        "1919"
        `shouldBe` Nothing
      Mega.parseMaybe
        (parser (Proxy :: Proxy (FieldValue BirthYear)))
        "2003"
        `shouldBe` Nothing
      Mega.parse
        (parser (Proxy :: Proxy (FieldValue Height)))
        "a"
        "76in"
        `shouldBe` Right 76
