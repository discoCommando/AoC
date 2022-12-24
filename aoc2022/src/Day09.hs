module Day09 where

import Board
import Common
import Control.Lens (view)
import Data.Data
import Data.List (nub)
import Debug.Trace (trace, traceShowId)
import Parseable
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega
import Prettyprinter (pretty)

-- data InputLine = InputLine {} deriving stock (Generic, Show, Eq)

type InputLine = (Direction, Int)

inputParser :: Parser InputLine
inputParser = do
  direction <- directionParser "UDLR"
  Mega.hspace
  value <- read <$> Mega.some Mega.digitChar
  pure (direction, value)

solution :: Solution [InputLine] Int Int
solution =
  Solution
    { parse = Mega.sepEndBy inputParser Mega.newline, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

moveTail :: Position -> Position -> Position
moveTail currentHead currentTail
  | abs difference.x <= 1 && abs difference.y <= 1 = currentTail
  | difference.x == 0 || difference.y == 0 = currentTail + (difference `divPosition` 2)
  | abs difference.x > 1 && abs difference.y <= 1 = currentTail + (difference `divPosition` Position 2 1)
  | abs difference.y > 1 && abs difference.x <= 1 = currentTail + (difference `divPosition` Position 1 2)
  | otherwise = currentTail + (difference `divPosition` 2)
  where
    difference = currentHead - currentTail

type Positions = [Position]

-- data State1 = State1 {
--               head :: Position,
--               tail :: Position,
--               positions :: Positions
--               }

moveBoth :: Direction -> Position -> Position -> Positions -> (Position, Position, Positions)
moveBoth direction (performMove direction -> newHead) (moveTail newHead -> newTail) ps = (newHead, newTail, newTail : ps)

moveLine :: InputLine -> Position -> Position -> Positions -> (Position, Position, Positions)
moveLine (_, 0) a b c = (a, b, c)
moveLine (d, i) a b c = let (a', b', c') = moveBoth d a b c in moveLine (d, i - 1) a' b' c'

part1' :: [InputLine] -> Int
part1' = length . nub . (\(_, _, c) -> c) . foldl (\(a, b, c) il -> moveLine il a b c) (0 :: Position, 0 :: Position, [])

moveBoth' :: Direction -> [Position] -> Positions -> ([Position], Positions)
moveBoth' direction tails ps =
  let newHead = performMove direction $ head tails
      newTails = reverse $ foldl (\newTails' tail' -> moveTail (head newTails') tail' : newTails') [newHead] $ tail tails
      diff = if null ps then newHead else let x = last newTails - head ps in if abs x.x >= 2 || abs x.y >= 2 then trace ("----- \n" ++ show (pretty (tails, newTails)) ++ "\n" ++ printMap tails ++ "\n" ++ printMap newTails) x else x
   in (seq diff newTails, last newTails : ps)

moveLine' :: InputLine -> [Position] -> Positions -> ([Position], Positions)
moveLine' (_, 0) a b = (a, b)
moveLine' (d, i) a b = let (a', b') = moveBoth' d a b in moveLine' (d, i - 1) a' b'

part2' :: [InputLine] -> Int
part2' input =
  let res = nub . snd $ foldl (\(a, b) il -> moveLine' il a b) (replicate 10 0, []) $ traceShowId input
   in length . trace (printMap res) $ res

main =
  aoc
    "09"
    solution
