{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
module Day2106 where

import Common
import Data.Data
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShowId)
import Parseable
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

type InputLine = [Int]

inputParser :: Parser InputLine
inputParser = Mega.sepEndBy Mega.decimal $ Mega.char ','

solution :: Solution InputLine Int Integer
solution =
  Solution
    { parse = inputParser,
      part1 = part1',
      part2 = part2'
    }

step :: [Int] -> [Int]
step fishes =
  let zeroes = filter (== 0) fishes
      newFishes =
        fmap
          ( \i ->
              if i == 0
                then 6
                else i - 1
          )
          fishes
   in newFishes ++ fmap (const 8) zeroes

grow :: Int -> (a -> a) -> a -> a
grow 0 f i = i
grow n f i = grow (n - 1) f $ f i

part1' :: InputLine -> Int
part1' = length . grow 80 step

type Map' = Map.Map Int Integer

createMap :: [Int] -> Map'
createMap = foldl (\m v -> Map.alter (Just . maybe 1 (+ 1)) v m) Map.empty

stepBetter :: Map' -> Map'
stepBetter m1 =
  let zeroesCount = fromMaybe 0 $ Map.lookup 0 m1
      counts = [0 .. 8]
   in foldl go Map.empty counts
  where
    go :: Map' -> Int -> Map'
    go m2 x =
      let count' = fromMaybe 0 $ Map.lookup x m1
          keys = if x == 0 then [6, 8] else [x - 1]
       in foldl (\m2' key -> Map.alter (Just . maybe count' (+ count')) key m2') m2 keys

count' :: Map' -> Integer
count' = sum . fmap snd . Map.toList

part2' :: InputLine -> Integer
part2' = count' . grow 256 stepBetter . createMap

main =
  aoc
    "2106"
    solution
