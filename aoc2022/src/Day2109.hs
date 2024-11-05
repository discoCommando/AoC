module Day2109 where

import Common
import Data.Data
import Data.Functor.Identity (runIdentity)
import Data.List (singleton)
import Debug.Trace (traceShowId)
import Parseable
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

{-
2199943210
3987894921
9856789892
8767896789
9899965678
    -}

type InputLine = [Int]

inputParser :: Parser InputLine
inputParser = Mega.some $ do
  read . singleton <$> Mega.digitChar

solution :: Solution [InputLine] Int Int
solution =
  Solution
    { parse = Mega.sepEndBy inputParser Mega.newline, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

type Board = [[Int]]

getAt :: Point -> Board -> Maybe Int
getAt (x, y) b
  | x < 0 || x >= length (head b) = Nothing
  | y < 0 || y >= length b = Nothing
  | otherwise = Just $ b !! y !! x

vectors = [(0, 1), (0, -1), (1, 0), (-1, 0)]

add :: Point -> Point -> Point
add (x, y) (a, b) = (x + a, y + b)

filter' :: (a -> Maybe b) -> [a] -> [b]
filter' f [] = []
filter' f ((f -> Nothing) : rest) = filter' f rest
filter' f ((f -> Just a) : rest) = a : filter' f rest

check :: Point -> Board -> Maybe Int
check p board = runIdentity $ do
  let current = unsafeMaybe $ getAt p board
  let points = filter' id $ (\v -> getAt (add p v) board) <$> vectors
  let allBigger = all (> current) points
  pure $ if allBigger then Just current else Nothing

getAllDips :: Board -> [Int]
getAllDips b = runIdentity $ do
  let len = length b
  let indexy = [0 .. len - 1]
  let indexx = [0 .. length (head b) - 1]
  let res = foldl (\res y -> foldl (\res' x -> maybe res' (: res') $ check (x, y) b) res indexx) [] indexy
  pure res

part1' :: [InputLine] -> Int
part1' = sum . traceShowId . fmap (+ 1) . getAllDips

part2' :: [InputLine] -> Int
part2' = const 1

main =
  aoc
    "2109"
    solution
