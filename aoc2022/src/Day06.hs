module Day06 where

import Common
import Data.Data
import Data.List (nub)
import Parseable
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

solution :: Solution [Char] Int Int
solution =
  Solution
    { parse = Mega.many Mega.letterChar, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

findFirstMarker :: Int -> [Char] -> Int
findFirstMarker i chars =
  let fourChars = take i chars
      isUnique = length (nub fourChars) == length fourChars
   in if isUnique
        then i
        else 1 + findFirstMarker i (tail chars)

part1' :: [Char] -> Int
part1' = findFirstMarker 4

part2' :: [Char] -> Int
part2' = findFirstMarker 14

main =
  aoc
    "06"
    solution
