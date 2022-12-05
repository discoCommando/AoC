module Day03 where

import Common
import Data.Char (isUpper)
import Data.Data
import Data.Foldable (toList)
import Data.List (intersect)
import Data.Sequence (chunksOf, fromList)
import Debug.Trace (traceShowId)
import Parseable
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

type InputLine' = Some NotWhitespace

type InputLine = String

inputParser :: Parser InputLine
inputParser = parser (Proxy :: Proxy InputLine')

solution :: Solution [InputLine] Int Int
solution =
  Solution
    { parse = Mega.sepEndBy inputParser Mega.newline, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

priority :: Char -> Int
priority c =
  if isUpper c
    then fromEnum c - fromEnum 'A' + (fromEnum 'z' - fromEnum 'a' + 1) + 1
    else fromEnum c - fromEnum 'a' + 1

part1' :: [InputLine] -> Int
part1' = sum . fmap go
  where
    go i =
      let length' = length i
          firstHalf = take (length' `div` 2) i
          secondHalf = drop (length' `div` 2) i
          intersection = head $ intersect firstHalf secondHalf
       in priority intersection

part2' :: [InputLine] -> Int
part2' = sum . fmap (go . toList) . toList . chunksOf 3 . fromList
  where
    go [a, b, c] =
      let intersection = head $ intersect a $ intersect b c
       in priority intersection

main =
  aoc
    "03"
    solution
