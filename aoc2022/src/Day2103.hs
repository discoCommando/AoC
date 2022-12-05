module Day2103 where

import Common
import Control.Arrow ((&&&))
import Data.Bits (shift)
import Data.Data
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.List (group, sort, transpose)
import Debug.Trace (traceShowId)
import Parseable
import Test.QuickCheck (listOf)
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

type InputLine' =
  SepEndBy
    ( Some
        ( Choice
            [ Const "0" False,
              Const "1" True
            ]
        )
    )
    Newline

type InputLine = [[Bool]]

inputParser :: Parser InputLine
inputParser = parser (Proxy :: Proxy InputLine')

solution :: Solution [InputLine] Int Int
solution =
  Solution
    { parse = Mega.sepEndBy inputParser Mega.newline, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

mostCommonOccurrence :: Ord a => [a] -> a
mostCommonOccurrence l =
  fst . maximumBy (compare `on` snd) $ elemCount
  where
    elemCount = map (head &&& length) . group . sort $ l

-- | first bit is the bit 0
-- ie [False, True] -> "10"
listOfBoolsToInt :: [Bool] -> Int
listOfBoolsToInt = \case
  [] -> 0
  (a : rest) -> (if a then 1 else 0) + shift (listOfBoolsToInt rest) 1

part1' :: [InputLine] -> Int
part1' =
  go . head
  where
    go i =
      let transposed = transpose i
          mostCommon = reverse $ fmap mostCommonOccurrence transposed
          leastCommon = fmap not mostCommon
          gamma = listOfBoolsToInt mostCommon
          epsilon = listOfBoolsToInt leastCommon
       in gamma * epsilon

getAtColumn :: Int -> [a] -> a
getAtColumn = flip (!!)

-- | counted from 0
getColumn :: Int -> [[a]] -> [a]
getColumn i = fmap $ getAtColumn i

part2' :: [InputLine] -> Int
part2' = go . head
  where
    go i =
      go2 0 True (>=) i * go2 0 False (<=) i
    go2 :: Int -> Bool -> (Int -> Int -> Bool) -> [[Bool]] -> Int
    go2 col priority op [a] = listOfBoolsToInt $ reverse a
    go2 col priority op as =
      let col' = getColumn col as
          priorityCount = length $ filter (== priority) col'
          restCount = length col' - priorityCount
       in go2 (col + 1) priority op $
            if priorityCount `op` restCount
              then filter ((== priority) . getAtColumn col) as
              else filter ((== not priority) . getAtColumn col) as

main =
  aoc
    "2103"
    solution
