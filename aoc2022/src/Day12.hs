{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use second" #-}
module Day12 where

import Board (Position, Visited, indexBoard)
import Common
import Control.Monad (join)
import Data.Char (ord)
import Data.Data
import qualified Data.Map.Strict as Map
import Parseable
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

type InputLine = String

data Map' = Map'
  { start :: Position,
    end :: Position,
    map :: Map.Map Position Elevation
  }

type Elevation = Int

inputParser :: Parser InputLine
inputParser = Mega.some Mega.alphaNumChar

type Visiteds = Map.Map Int Visited

solution :: Solution [InputLine] Int Int
solution =
  Solution
    { parse = Mega.sepEndBy inputParser Mega.newline, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

inputToMap' :: [InputLine] -> Map'
inputToMap' input' =
  let indexed' = join $ indexBoard input'
      start = fst . head $ filter (\(_, c) -> c == 'S') indexed'
      end = fst . head $ filter (\(_, c) -> c == 'E') indexed'
      map = Map.fromList $ fmap (\(p, c) -> (p, ord c)) indexed'
   in Map' {..}

part1' :: [InputLine] -> Int
part1' = const 1

part2' :: [InputLine] -> Int
part2' = const 1

main =
  aoc
    "12"
    solution
