{-# LANGUAGE RecordWildCards #-}

module Day2104 where

import Common
import Control.Lens
import Control.Monad.ST
import Data.Data
import Data.List (transpose)
import Parseable
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega
import Debug.Trace (traceShowId)

type Guesses' = SepEndBy (ToInt (Some Digit)) (Chunk ",") <$ Newline

type Board' =
  Newline
    $> Sized
         5
         ( Many Space $> (SepEndBy (ToInt (Some Digit)) (Many Space)
             <$ Newline)
         )

type InputLine' = Guesses' :+: Many Board'

type RawBoard = [[Int]]

data InputLine = InputLine
  { guesses :: [Int],
    boards :: [RawBoard]
  }
  deriving stock (Generic)

inputParser :: Parser InputLine
inputParser = do
  (guesses, boards) <- parser (Proxy :: Proxy InputLine')
  pure InputLine {..}

solution :: Solution [InputLine] Int Int
solution =
  Solution
    { parse = Mega.sepEndBy inputParser Mega.newline, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

data Cell = Cell {visited :: Bool, value :: Int} deriving stock (Generic)

type Board = [[Cell]]

isRowFinished :: [Cell] -> Bool
isRowFinished = all (\cell -> cell.visited)

isBoardFinished :: Board -> Bool
isBoardFinished b =
  let finishedRows = filter isRowFinished b
      finishedColumns = filter isRowFinished $ transpose b
   in not (null finishedRows && null finishedColumns)

boardScore :: Int -> Board -> Int
boardScore justCalledNumber =
  (* justCalledNumber)
    . sum
    . fmap (\cell -> cell.value)
    . filter (\cell -> not $ cell.visited)
    . mconcat

crossOutGuess :: Int -> Board -> Board
crossOutGuess guess = fmap $ fmap (\c -> if c.value == guess then c {visited = True} else c)

part1' :: [InputLine] -> Int
part1' (head -> input) =
  let boards = fmap (fmap (Cell False)) <$> traceShowId input.boards
   in go input.guesses boards
  where
    go guesses boards =
      let guess = traceShowId $ head guesses
          result = crossOutGuess guess <$> boards
          finishedBoards = filter isBoardFinished result
       in if not (null finishedBoards)
            then boardScore guess $ head finishedBoards
            else go (tail guesses)  result

part2' :: [InputLine] -> Int
part2' (head -> input) =
  let boards = fmap (fmap (Cell False)) <$> traceShowId input.boards
   in go input.guesses boards
  where
    go guesses boards =
      let guess = traceShowId $ head guesses
          result = crossOutGuess guess <$> boards
          newBoards = filter (not . isBoardFinished) result
       in if null newBoards
            then boardScore guess $ head result
            else go (tail guesses)  newBoards

main =
  aoc
    "2104"
    solution
