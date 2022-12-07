module Day2107 where

import Common
import Data.Data
import Debug.Trace
import Parseable
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (space)
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

type InputLine = [Int]

inputParser :: Parser InputLine
inputParser = undefined

solution :: Solution InputLine Int Int
solution =
  Solution
    { parse = sepEndBy Mega.decimal (Mega.char ','), -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

part1' :: InputLine -> Int
part1' i =
  let min' = minimum i
      max' = maximum i
      sum2 = (\avg -> sum $ (\a -> abs $ a - avg) <$> i) <$> [min' .. max']
   in minimum sum2

arithmeticSum :: Int -> Int -> Int
arithmeticSum i1 i2 = (1 + abs (i1 - i2)) * abs (i1 - i2) `div` 2

part2' :: InputLine -> Int
part2' i =
  let min' = minimum i
      max' = maximum i
      sum2 = (\avg -> sum $ arithmeticSum avg <$> i) <$> [min' .. max']
   in minimum sum2

main =
  aoc
    "2107"
    solution
