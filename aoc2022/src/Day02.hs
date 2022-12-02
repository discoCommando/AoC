{-# LANGUAGE TypeFamilies #-}

module Day02 where

import Common
import Data.Data
import Debug.Trace (traceShowId)
import Parseable
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

data Piece = Rock | Paper | Scissors deriving stock (Show, Eq, Generic, Enum, Bounded)

instance KnownValue Rock where
  type Val Rock = Piece
  valVal _ = Rock

instance KnownValue Paper where
  type Val Paper = Piece
  valVal _ = Paper

instance KnownValue Scissors where
  type Val Scissors = Piece
  valVal _ = Scissors

type InputLine' =
  ( Choice
      [ Const "A" Rock,
        Const "B" Paper,
        Const "C" Scissors
      ]
      <$ Space
  )
    :+: Choice
          [ Const "X" Rock,
            Const "Y" Paper,
            Const "Z" Scissors
          ]

type InputLine = (Piece, Piece)

inputParser :: Parser InputLine
inputParser = parser (Proxy :: Proxy InputLine')

data Outcome = Win | Lose | Draw deriving stock (Show, Eq, Generic, Enum, Bounded)

solution :: Solution [InputLine] Int Int
solution =
  Solution
    { parse = Mega.sepEndBy inputParser Mega.newline, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

pointForPiece :: Piece -> Int
pointForPiece = \case
  Rock -> 1
  Paper -> 2
  Scissors -> 3

outcome :: Piece -> Piece -> Outcome
outcome Paper Rock = Win
outcome Scissors Paper = Win
outcome Rock Scissors = Win
outcome a b
  | a == b = Draw
  | otherwise = Lose

outcomeToInt :: Outcome -> Int
outcomeToInt = \case
  Win -> 6
  Draw -> 3
  Lose -> 0

totalScore :: Piece -> Piece -> Int
totalScore opponent mine = outcomeToInt (outcome mine opponent) + pointForPiece mine

part1' :: [InputLine] -> Int
part1' = sum . fmap (uncurry totalScore)

-- im lazy
pieceToOutcome :: Piece -> Outcome
pieceToOutcome = \case
  Rock -> Lose
  Paper -> Draw
  Scissors -> Win

choosePiece :: Piece -> Outcome -> Piece
choosePiece opponent wantedOutcome =
  head $
    filter (\mine -> outcome mine opponent == wantedOutcome) [minBound .. maxBound :: Piece]

part2' :: [InputLine] -> Int
part2' =
  sum
    . fmap
      ( \(opponent, hack) ->
          totalScore opponent $
            choosePiece opponent $ pieceToOutcome hack
      )

main =
  aoc
    "02"
    solution
