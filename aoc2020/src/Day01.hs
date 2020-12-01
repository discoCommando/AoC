module Day01 where

import Common
import Data.Maybe

solution =
  Solution
    { parse = fmap toInt . lines, -- No parsing required.
      part1 = solution1,
      part2 = solution2
    }

main =
  aoc
    "01"
    solution

solution1Helper :: [Int] -> [Maybe Int]
solution1Helper input = do
  x <- input
  y <- input
  pure $
    if x + y == 2020
      then Just $ x * y
      else Nothing

solution1 :: [Int] -> Int
solution1 = head . catMaybes . solution1Helper

solution2Helper :: [Int] -> [Maybe Int]
solution2Helper input = do
  x <- input
  y <- input
  z <- input
  pure $
    if x + y + z == 2020
      then Just $ x * y * z
      else Nothing

solution2 :: [Int] -> Int
solution2 = head . catMaybes . solution2Helper
