module Day05 where

import Common
import Data.Data
import Parseable
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega
import Debug.Trace

type Crate' = (Chunk "[" $> Letter <$ Chunk "]") <||> Chunk "   "

type Crates' = SepEndBy Crate' Space

type AllCrates' = SepEndBy Crates' Newline

type Numbers' = Many Space $> SepEndBy Digit (Many Space)

type Move' =
  ( Chunk "move "
      $> Decimal
  )
    :+: ( Chunk " from "
            $> Decimal
        )
    :+: ( Chunk " to "
            $> Decimal
        )

type Moves' = SepEndBy Move' Newline

type InputLine' = (AllCrates' <$ (Numbers' :+: Newline :+: Newline)) :+: Moves'

type Crate = Char

data Move = Move
  { count :: Int,
    from :: Int,
    to :: Int
  }
  deriving stock (Generic, Show, Eq)

data InputLine = InputLine
  { crates :: [[Crate]],
    moves :: [Move]
  }
  deriving stock (Generic, Show, Eq)

applyAt :: (a -> a) -> Int -> [a] -> [a]
applyAt f 0 (x : rest) = f x : rest
applyAt f n (x : rest) = x : applyAt f (n - 1) rest

rawCratesToCrates :: [[Either Char ()]] -> [[Crate]]
rawCratesToCrates rawCrates =
  let longest = maximum $ length <$> rawCrates
      emptyCrates = [[] | _ <- [0 .. longest - 1]]
   in reverse <$> foldl
        ( \crates ->
            snd
              . foldl
                ( \(i, crates') -> \case
                    Right () -> (i + 1, crates')
                    Left c -> (i + 1, applyAt (c :) i crates')
                )
                (0, crates)
        )
        emptyCrates
        rawCrates

rawMovesToMoves :: [((Int, Int), Int)] -> [Move]
rawMovesToMoves = fmap (\((count, from), to) -> Move {..})

inputParser' :: Parser ([[Either Char ()]], [((Int, Int), Int)])
inputParser' = parser (Proxy :: Proxy InputLine')

inputParser :: Parser InputLine
inputParser = do
  (rawCrates, rawMoves) <- parser (Proxy :: Proxy InputLine')
  let crates =  rawCratesToCrates rawCrates
  let moves =  rawMovesToMoves rawMoves
  pure InputLine {..}

solution :: Solution InputLine String String
solution =
  Solution
    { parse = inputParser, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

getResult :: [[Crate]] -> String
getResult [] = []
getResult (h : rest) = if null h then getResult rest else head h : getResult rest

makeMove :: Move -> [[Crate]] -> [[Crate]]
makeMove Move {..} cratesList =
  if count == 0
    then cratesList
    else
      makeMove Move {count = count - 1, ..} $
        applyAt tail (from - 1) $
          applyAt (head (cratesList !! (from - 1)) :) (to - 1) cratesList

part1' :: InputLine -> String
part1' InputLine {..} =
  if null moves
    then getResult crates
    else part1'  InputLine {moves = tail moves, crates = makeMove (head moves) crates}

makeMove2 :: Move -> [[Crate]] -> [[Crate]]
makeMove2 Move {..} cratesList = 
        applyAt (drop count) (from - 1) $
          applyAt (take count (cratesList !! (from - 1)) ++ ) (to - 1) cratesList

part2' :: InputLine -> String
part2' InputLine {..} =
  if null moves
    then getResult crates
    else part2'  InputLine {moves = tail moves, crates = makeMove2 (head moves) crates}

main =
  aoc
    "05"
    solution
