module Day2110 where

import Common
import Data.Data
import Parseable
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega
import qualified Data.Map.Strict as Map

bracketMap = Map.fromList [
                          ('<', '>'),
                          ('[', ']'),
                          ('{', '}'),
                          ('(', ')')
                          ]

bracketPoints = Map.fromList [
                          ( '>', 25137),
                          ( ']', 57),
                          ( '}', 1197),
                          ( ')', 3)
                          ]

type InputLine = String

inputParser :: Parser InputLine
inputParser = Mega.some  Mega.punctuationChar

solution :: Solution [InputLine] Int Int
solution =
  Solution
    { parse = Mega.sepEndBy inputParser Mega.newline, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }


part1' :: [InputLine] -> Int
part1' = const 1

part2' :: [InputLine] -> Int
part2' = const 1

main =
  aoc
    "2110"
    solution
