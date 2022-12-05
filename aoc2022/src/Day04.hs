{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use ||" #-}
module Day04 where

import Common
import Data.Data
import Debug.Trace (traceShowId)
import Parseable
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

type Sep' = Choice [Chunk ",", Chunk "-"]

type InputLine' = ToInt (Some Digit) <$ Sep' :+: SepEndBy (ToInt (Some Digit)) Sep'

data InputLine = InputLine {x1 :: Int, y1 :: Int, x2 :: Int, y2 :: Int} deriving stock (Generic, Show, Eq)

inputParser :: Parser InputLine
inputParser = do
  (x1, l) <- parser (Proxy :: Proxy InputLine')
  pure InputLine {x1 = x1, y1 = head l, x2 = l !! 1, y2 = l !! 2}

solution :: Solution [InputLine] Int Int
solution =
  Solution
    { parse = Mega.sepEndBy inputParser Mega.newline, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

isBetween :: Int -> (Int, Int) -> Bool
isBetween i (a, b) = i >= a && i <= b

containsAnother :: InputLine -> Bool
containsAnother InputLine {..} =
  or
    [ x2
        `isBetween` (x1, y1)
        && y2
        `isBetween` (x1, y1),
      x1
        `isBetween` (x2, y2)
        && y1
        `isBetween` (x2, y2)
    ]

part1' :: [InputLine] -> Int
part1' = length . filter containsAnother

overlap :: InputLine -> Bool
overlap InputLine {..} =
  or
    [ x2 `isBetween` (x1, y1),
      y2 `isBetween` (x1, y1),
      x1 `isBetween` (x2, y2),
      y1 `isBetween` (x2, y2)
    ]

part2' :: [InputLine] -> Int
part2' = length . filter overlap

main =
  aoc
    "04"
    solution
