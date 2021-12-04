module Day15 where

import qualified Board
import Common
import Control.Lens
import Control.Monad.State
import Data.Data
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShowId)
import Parseable
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

newtype InputLine = InputLine {v :: Int} deriving stock (Generic, Show, Eq)

inputParser :: Parser InputLine
inputParser = InputLine <$> Mega.decimal

solution :: Solution [InputLine] Int Int
solution =
  Solution
    { parse = Mega.sepEndBy inputParser $ Mega.chunk ",", -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

data State1 = State1
  { map :: Map.Map Int [Int],
    last :: Int
  }
  deriving stock (Generic)

part1' :: [InputLine] -> Int
part1' ls =
  let initState =
        State1
          { map = Map.fromList . fmap (\(i, il) -> (il ^. #v, [i + 1])) . Board.indexed $ ls,
            last = Prelude.last ls ^. #v
          }
   in flip evalState initState $ part1Helper (length ls + 1)

part1Helper :: Int -> State State1 Int
part1Helper round = do
  last <- use #last
  v <- uses #map (fromMaybe [] . (Map.!? last))
  let newLast = case v of
        (x : y : _) -> x - y
        _ -> 0
  #last .= newLast
  v2 <- uses #map (fromMaybe [] . (Map.!? newLast))
  #map %= Map.insert newLast (round : v2)
  if round == 2020
    then pure newLast
    else part1Helper $ round + 1

part2' :: [InputLine] -> Int
part2' = const 1

main =
  aoc
    "15"
    solution
