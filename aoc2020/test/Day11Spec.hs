module Day11Spec where

import Board (Position (..))
import Common
import Control.Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import Day11 hiding (main)
import Test.Hspec
import Text.RawString.QQ (r)

example1 =
  unsafeParseExample
    solution
    [r|L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL|]

example2 =
  unsafeParseExample
    solution
    [r|.##.##.
#.#.#.#
##...##
...L...
##...##
#.#.#.#
.##.##.|]

example3 =
  unsafeParseExample
    solution
    [r|.##.##L
###.#.#
##.#.##
.#.L..#
##..###
#.#.#.#
#######|]

example4 =
  unsafeParseExample
    solution
    [r|#..#
....
....
#..#|]

spec =
  describe "all" $ do
    it "all" $ do
      True `shouldBe` True
      False `shouldBe` False
      part1 solution example1 `shouldBe` 37
      part2 solution example1 `shouldBe` 26
      (view #neighbours <$> ((initState example2 ^. #board) Map.!? (Position 3 3))) `shouldBe` Just Set.empty
      (Set.size . view #neighbours <$> ((initState example3 ^. #board) Map.!? (Position 3 3))) `shouldBe` Just 8
      (view #neighbours <$> ((initState example4 ^. #board) Map.!? (Position 0 0))) `shouldBe` Just (Set.fromList [Position 3 3, Position 0 3, Position 3 0])
