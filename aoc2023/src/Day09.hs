module Day09 where

import Common
import Data.Data
import Parseable
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

type InputLine = [Int]

inputParser :: Parser InputLine
inputParser = Mega.sepBy pInt spaces

solution :: Solution [InputLine] Int Int
solution =
  Solution
    { parse = Mega.sepEndBy inputParser Mega.newline, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

diffs :: [Int] -> [Int]
diffs [] = []
diffs [_] = []
diffs (x : y : xs) = (y - x) : diffs (y : xs)

allZeroes [] = True
allZeroes (0 : xs) = allZeroes xs
allZeroes _ = False

diffUntilZeroes :: [Int] -> [[Int]]
diffUntilZeroes xs =
  if allZeroes xs
    then [xs]
    else
      let d = diffs xs
       in xs : diffUntilZeroes d

findP1 :: [Int] -> Int
findP1 i =
  let d = diffUntilZeroes i
   in sum $ last <$> d

part1' :: [InputLine] -> Int
part1' = sum . fmap findP1

part2' :: [InputLine] -> Int
part2' = sum . fmap (findP1 . reverse)

main =
  aoc
    "09"
    solution
