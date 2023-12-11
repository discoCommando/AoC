module Day01Spec where

import Common
import Data.Maybe (isJust, mapMaybe)
import Day01 hiding (main)
import GHC.IO (liftIO)
import NumberHelper (numberFromList)
import Test.Hspec
import Test.QuickCheck

-- import Text.RawString.QQ (r)

-- example1 =
--   unsafeParseExample
--     solution
--     [r| |]

data Case = I Int | S Int | X Char
  deriving (Show, Eq)

data Ipt = Ipt [Case]
  deriving (Show, Eq)

toInt' :: Case -> Maybe Int
toInt' = \case
  I i -> Just i
  S i -> Just i
  X _ -> Nothing

toString :: Case -> String
toString = \case
  I i -> show i
  S i -> case i of
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"
    _ -> undefined
  X c -> [c]

instance Arbitrary Case where
  arbitrary = do
    let ns = oneof (pure <$> [1 .. 9])
    i <- ns
    s <- ns
    x <- oneof (pure <$> ['a' .. 'z'])
    elements [I i, S s, X x]

instance Arbitrary Ipt where
  arbitrary = do
    cases <- listOf arbitrary `suchThat` (\x -> not $ null $ filter isJust $ toInt' <$> x) `suchThat` (\x -> length x < 10)
    pure $ Ipt cases

properResult :: Ipt -> Integer
properResult (Ipt is) =
  let dgs = mapMaybe id $ (toInt' <$> is)
   in numberFromList $ reverse $ [(head dgs), (last dgs)]

part2Result :: Ipt -> Integer
part2Result (Ipt is) =
  let s = concatMap toString is
      dgs = part2' [InputLine s]
   in dgs

spec =
  describe "all" $ do
    it "all" $ do
      putStrLn "chuj"
      True `shouldBe` True
      False `shouldBe` False
    it "qc" $ do
      property $ \ipt -> properResult ipt == part2Result ipt
