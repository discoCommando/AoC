module Day03Spec where

import Common
import Day03 hiding (main)
import Debug.Trace (trace)
import Test.Hspec
import Text.Megaparsec.Char as Mega
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

example1Parsed = unsafeParseExample solution example1

line = unsafeParse

spec =
  describe "all" $ do
    it "all" $ do
      unsafeParse tileParser "." `shouldBe` Empty
      unsafeParse tileParser "#" `shouldBe` Tree
      unsafeParse (takeWhen tileParser) ".#.#" `shouldBe` [Empty, Tree, Empty, Tree]
      unsafeParse (listParser Mega.newline (takeWhen tileParser)) ".#.\n#.." `shouldBe` [[Empty, Tree, Empty], [Tree, Empty, Empty]]
      part1 solution example1Parsed `shouldBe` 7
      part2 solution example1Parsed `shouldBe` 336
