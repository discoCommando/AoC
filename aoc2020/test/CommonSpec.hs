module CommonSpec where

import Common
import Control.Monad (join)
import Test.Hspec
import Test.QuickCheck
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MegaC
import qualified Text.Megaparsec.Char.Lexer as Mega
import Text.RawString.QQ (r)

example1 =
  [r|..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#|]

spec = do
  describe "all" $ do
    it "parseStringAs" $ do
      quickCheck $ \(s :: String) ->
        Mega.parse (parseStringAs s ()) "a" s `shouldBe` Right ()
    it "listParser" $ do
      quickCheck $ \(ss :: [Int]) ->
        Mega.parse (listParser MegaC.newline $ Mega.signed (pure ()) Mega.decimal) "a" (unlines $ fmap show ss) `shouldBe` Right ss
      Mega.parse (listParser MegaC.newline $ Mega.many $ Mega.choice [parseStringAs "." '.', parseStringAs "#" '#']) "a" example1 `shouldBe` Right (lines example1)
