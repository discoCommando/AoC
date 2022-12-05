module Day2105 where

import Board
import Common
import Control.Monad.ST
import Data.Data
import Data.Foldable (maximumBy, minimumBy)
import qualified Data.Map.Strict as Map
import Debug.Trace (traceShowId)
import Parseable
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

type Number = ToInt (Some Digit)

type InputLine' = (Number <$ Chunk ",") :+: (Number <$ Chunk " -> ") :+: (Number <$ Chunk ",") :+: Number

data InputLine = InputLine {x1 :: Int, y1 :: Int, x2 :: Int, y2 :: Int} deriving stock (Generic, Show, Eq)

inputParser :: Parser InputLine
inputParser = do
  (((x1, y1), x2), y2) <- parser (Proxy :: Proxy InputLine')
  pure InputLine {..}

solution :: Solution [InputLine] Int Int
solution =
  Solution
    { parse = Mega.sepEndBy inputParser Mega.newline, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

haveSameCoordinates :: InputLine -> Bool
haveSameCoordinates InputLine {..} = x1 == x2 || y1 == y2

newtype Cell = Cell {count :: Int} deriving stock (Generic, Show)

type Map' = Map.Map (Int, Int) Cell

getVector :: Int -> Int -> Int
getVector i1 i2 = if i1 == i2 then 0 else signum (i1 - i2) * (-1)

createLine :: InputLine -> [(Int, Int)]
createLine InputLine {..} =
  let lx = x1 - x2
      ly = y1 - y2
      vx = getVector x1 x2
      vy = getVector y1 y2
      length' = max (abs lx) (abs ly)
   in [(x1 + vx * i, y1 + vy * i) | i <- [0 .. length']]

part1' :: [InputLine] -> Int
part1' = part2' . filter haveSameCoordinates

-- part1' input = runST $ do
--   let minx = minimumBy (\i1 i2 -> min i1.x1 i1.x2 `compare` min i2.x1 i2.x2) input
--   let miny = minimumBy (\i1 i2 -> min i1.y1 i1.y2 `compare` min i2.y1 i2.y2) input
--   let maxx = maximumBy (\i1 i2 -> max i1.x1 i1.x2 `compare` max i2.x1 i2.x2) input
--   let maxy = maximumBy (\i1 i2 -> max i1.y1 i1.y2 `compare` max i2.y1 i2.y2) input
--   board <- newBoard $ [ fmap (const $ Cell 0) [minx.x..maxx.x] | y <- [miny.y..maxy.y]]
--   pure 1

part2' :: [InputLine] -> Int
part2' =
  go Map.empty
  where
    go :: Map' -> [InputLine] -> Int
    go map' [] =
      length $
        filter (\(_, cell) -> cell.count >= 2) $
          Map.toList map'
    go map' (inputLine : rest) =
      let line = createLine inputLine
          newMap = foldr (Map.alter (Just . maybe (Cell 1) (\cell -> cell {count = cell.count + 1}))) map' line
       in go newMap rest

main =
  aoc
    "2105"
    solution
