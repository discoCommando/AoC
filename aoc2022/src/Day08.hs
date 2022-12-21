module Day08 where

import Common
import Control.Lens (Identity (runIdentity))
import Data.Data
import Data.List
import Parseable
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

data Cell = Cell {value :: Int, visible :: CellState}
  deriving stock (Generic, Show, Eq)

type InputLine = [Cell]

inputParser :: Parser InputLine
inputParser = Mega.some $ do
  value <- read . singleton <$> Mega.digitChar
  pure Cell {visible = NotVisible, ..}

data CellState = Visible | NotVisible
  deriving stock (Generic, Show, Eq)

solution :: Solution [InputLine] Int Int
solution =
  Solution
    { parse = Mega.sepEndBy inputParser Mega.newline, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

type Board = PrimitiveBoard Cell

reverseVector :: Point -> Point
-- reverseVector (x, y) = (x * -1, y * -1)
reverseVector = id

{- we know that all the previous cells are visible
   -}
viewFromTheSide :: Point -> Point -> Board -> Board
viewFromTheSide current vector board = runIdentity $ do
  let Just currentValue = getAtBoard current board
  if currentValue.visible == Visible
    then pure board
    else do
      let previous = getAtBoard (reverseVector vector)
      pure board

part1' :: [InputLine] -> Int
part1' = const 1

part2' :: [InputLine] -> Int
part2' = const 1

main =
  aoc
    "08"
    solution
