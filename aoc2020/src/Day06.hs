module Day06 where

import Common
import Control.Monad (join)
import Data.Data
import Data.Set
import Parseable
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

type InputParser = SepEndBy (SepEndBy (Some NotWhitespace) Newline) Newline

part1' :: [[[Char]]] -> Int
part1' = sum . fmap (Data.Set.size . Data.Set.fromList . join)

allLetters = Data.Set.fromList ['a' .. 'z']

part2' :: [[[Char]]] -> Int
part2' = sum . fmap (Data.Set.size . Prelude.foldl Data.Set.intersection allLetters . fmap Data.Set.fromList)

solution :: Solution [[[Char]]] Int Int
solution =
  Solution
    { parse = parser (Proxy :: Proxy InputParser), -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

main =
  aoc
    "06"
    solution
